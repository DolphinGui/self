#include <algorithm>
#include <bits/iterator_concepts.h>
#include <cctype>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/tuple.hpp"
#include "ast/unevaluated_expression.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "pair_range.hpp"
#include "scope_guard.hpp"

namespace {
auto isInt(self::TokenView t) {
  char *p;
  auto number = std::strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}

void escape(self::Token &literal) {
  for (auto c = literal.begin(); c != literal.end(); ++c) {
    if (*c == '\\') {
      auto mark = c;
      ++c;
      if (std::isdigit(*c)) {
        char *end;
        auto value = std::strtol(&*c, &end, 10);
        if (value > std::numeric_limits<unsigned char>::max()) {
          throw std::runtime_error("escaped char value is too big.");
        }
        // this is a stupid workaround but I guess it'll work
        auto distance = std::distance(&*c, end);
        literal.erase(c, c + 1 + distance);
        *mark = value;
      } else if (*c == 'n') {
        *mark = '\n';
        literal.erase(c);
      }
    }
  }
}

auto isChar(self::TokenView t) {
  if (t.front() != '\'')
    return std::pair{false, (unsigned char)'\0'};
  auto literal = std::string(t);
  escape(literal);
  if (literal.length() != 3)
    return std::pair{false, (unsigned char)'\0'};
  return std::pair{true, (unsigned char)literal.at(1)};
}

std::pair<bool, std::string> isStr(self::TokenView t) {
  if (t.front() != '\'' && t.front() != '\"')
    return {false, ""};
  if (!t.ends_with('\'') && !t.ends_with('\"'))
    return {false, ""};
  auto literal = std::string(t);
  escape(literal);
  return {true, std::string(literal.substr(1, literal.length() - 2))};
}

std::pair<bool, bool> isBool(self::TokenView t) {
  if (t == "true")
    return {true, true};
  else if (t == "false")
    return {true, false};
  else
    return {false, false};
}

constexpr auto notReserved = [](auto t) {
  return !self::reserved::isKeyword(t) && !self::reserved::isGrammar(t);
};

template <typename T> struct TokenItBase {
  T &where;
  size_t pos = 0;
  self::TokenView operator*() { return where.at(pos); }
  TokenItBase &operator++() {
    if (pos >= where.size()) {
      throw std::runtime_error("out of bounds");
    }
    ++pos;
    return *this;
  }
  TokenItBase operator++(int) {
    TokenItBase tmp = *this;
    this->operator++();
    return tmp;
  }
  bool end() const noexcept { return pos == where.size(); }
};
using TokenIt = TokenItBase<std::vector<self::Token>>;

struct GlobalParser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  using TypeList = std::unordered_map<self::TokenView, self::TypeRef>;
  self::SymbolMap global;
  self::Context &c;
  // todo: add proper compile error reporting later.
  auto errReport(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }

  static void processSubtrees(self::ExprTree &tree, auto begin,
                              auto end) {
    for (auto open = begin; open != end; ++open) {
      if (auto *open_paren =
              dynamic_cast<self::UnevaluatedExpression *>(open->get());
          open_paren && open_paren->getToken() == "(") {
        for (auto close = 1 + open; close != end; ++close) {
          if (auto *close_paren =
                  dynamic_cast<self::UnevaluatedExpression *>(close->get())) {
            if (close_paren->getToken() == "(") {
              processSubtrees(tree, close, end);
            } else if (close_paren->getToken() == ")") {
              auto subtree = std::make_unique<self::ExprTree>();
              subtree->reserve(std::distance(open + 1, close));
              std::for_each(open + 1, close, [&](self::ExprPtr &e) {
                subtree->emplace_back(std::move(e));
              });
              tree.erase(open, close + 1);
              tree.insert(open, std::move(subtree));
            }
          }
        }
      }
    }
  }

  static void processSubtrees(self::ExprTree &tree) {
    return processSubtrees(tree, tree.begin(), tree.end());
  }

  void processTuples(self::ExprTree &tree, self::SymbolMap &context,
                     self::SymbolMap &global) {
    size_t size = 0;
    auto tuple = std::make_unique<self::Tuple>();
    auto mark = tree.begin();
    for (auto it = tree.begin(); it != tree.end(); ++it, ++size) {
      if (auto *comma = dynamic_cast<self::UnevaluatedExpression *>(it->get());
          comma && comma->getToken() == ",") {
        self::ExprTree branch;
        branch.reserve(size);
        std::for_each(mark, it, [&](self::ExprPtr &ptr) {
          branch.push_back(std::move(ptr));
        });
        mark = it = tree.erase(mark, it + 1);
        if (branch.size() > 1) {
          tuple->members.emplace_back(evaluateTree(branch, context));
        } else {
          tuple->members.emplace_back(
              parseSymbol(branch.back(), context, global));
        }
        size = 0;
      }
    }
    if (!tuple->members.empty()) {
      if (tree.size() > 1) {
        tuple->members.emplace_back(evaluateTree(tree, context));
      } else {
        tuple->members.emplace_back(parseSymbol(tree.back(), context, global));
        tree.pop_back();
      }
      tuple->members.shrink_to_fit();
      tree.push_back(std::move(tuple));
    }
  }

  enum struct coerce_result { match, coerce, mismatch };
  coerce_result need_coerce(const self::ExprBase *e, self::TypePtr type) {
    using enum coerce_result;
    if (const auto *var = dynamic_cast<const self::VarDeclaration *>(e)) {
      if (!var->type_ref.ptr) {
        return coerce;
      }
    } else if (const auto *uneval =
                   dynamic_cast<const self::UnevaluatedExpression *>(e)) {
      return coerce;
    }
    if (e->getType() == type)
      return match;
    else
      return mismatch;
  }

  static bool coerceType(self::ExprBase *e, self::TypePtr type) {
    if (auto *var = dynamic_cast<self::VarDeclaration *>(e)) {
      if (!var->type_ref.ptr) {
        var->type_ref = type;
        return true;
      }
    } else if (auto *uneval = dynamic_cast<self::UnevaluatedExpression *>(e)) {
      uneval->coerced_type = type;
      return true;
    }
    return e->getType() == type;
  }

  static size_t tuple_count(self::ExprBase *e) {
    if (auto *tuple = dynamic_cast<self::Tuple *>(e))
      return tuple->members.size();
    return 1;
  }

  static void for_tuple(self::ExprBase *e, auto unary) {
    if (auto *tuple = dynamic_cast<self::Tuple *>(e)) {
      for (auto &a : tuple->members) {
        unary(*a);
      }
    }
    unary(*e);
  }

  static void for_tuple(self::ExprBase *left, auto gen, auto binary) {
    if (auto *tuple = dynamic_cast<self::Tuple *>(left)) {
      for (auto l = tuple->members.begin(); l != tuple->members.end(); ++l) {
        binary(**l, gen());
      }
      throw std::runtime_error("for tuple mismatch");
    }
    binary(*left, gen());
  }

  template <typename T, bool pre = true, bool post = true>
  auto processFunction(auto &it, bool not_a_member, auto lhsrhsinc, auto cond,
                       auto cleanup, auto insert, auto coerce,
                       self::SymbolMap &context) {
    if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      auto candidates =
          self::pair_range(global.equal_range(T::mangle(t->getToken())));
      std::vector<const T *> no_coerce;
      std::vector<const T *> coerced;
      for (auto &[_, fun] : candidates) {
        const auto &op = dynamic_cast<const T &>(fun.get());
        if (not_a_member ^ op.member) {
          cond(&op, lhs, rhs, no_coerce, coerced);
        }
      }
      if (&global != &context) {
        auto local_candidates =
            self::pair_range(context.equal_range(T::mangle(t->getToken())));
        for (auto &[_, fun] : local_candidates) {
          const auto &op = dynamic_cast<const T &>(fun.get());
          if (not_a_member ^ op.member) {
            cond(&op, lhs, rhs, no_coerce, coerced);
          }
        }
      }
      if (!no_coerce.empty()) {
        errReport(no_coerce.size() == 1, "ambiguous operator call");
        auto result = insert(no_coerce.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      } else if (!coerced.empty()) {
        errReport(coerced.size() == 1, "ambiguous operator call");
        coerce(*coerced.back(), *lhs, *rhs);
        auto result = insert(coerced.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      }
    }
  };
  self::ExprPtr evaluateTree(self::ExprTree &tree,
                                   self::SymbolMap &local) {
    if (tree.empty()) {
      return std::make_unique<self::Tuple>();
    }
    processSubtrees(tree);
    for (auto &ptr : tree) {
      if (auto *uneval = dynamic_cast<self::ExprTree *>(ptr.get())) {
        ptr = evaluateTree(*uneval, local);
      }
    }
    auto a = tree.dump();
    processTuples(tree, local, global);
    for (auto &ptr : tree) {
      ptr = parseSymbol(ptr, local, global);
    } // clang-format off
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wunused-parameter"
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      processFunction<self::FunctionDef>(
          it, true, [](auto &lhs, auto &rhs) { ++rhs; },
          [](auto *fun, auto lhs, auto rhs,
             auto &perfect, auto& less_than) {
            if (fun->argcount() == tuple_count(rhs->get())) {
              perfect.push_back(fun);
            }
          },
          [&](auto lhs, auto rhs) { tree.erase(rhs); },
          [&](const self::FunctionDef *fun, auto lhs, auto rhs) {
            auto result = std::make_unique<self::FunctionCall>(*fun);
            if (auto *tuple = dynamic_cast<self::Tuple *>(rhs->get())) {
              result->rhs =
                  std::make_unique<self::arg_pack>(std::move(*tuple));
            } else {
              result->rhs = std::move(*rhs);
            }
            return result;
          }, [](const self::FunctionDef& fun, auto& lhs, auto& rhs){
            int i = 0;
            for_tuple(rhs.get(), [&]{
              return fun.arguments.at(i++)->getDecltype();},
              [](self::ExprBase& e, auto type){coerceType(&e, type);});
          }, local);
    }
    #pragma clang diagnostic pop // clang-format on

    auto binCondition = [&](auto *op, auto lhs, auto rhs, auto &no_coerce,
                            auto &coerce_r) {
      using enum coerce_result;
      auto left = need_coerce(lhs->get(), op->rhs->getDecltype());
      auto right = need_coerce(rhs->get(), op->rhs->getDecltype());
      if (left == match && right == match) {
        no_coerce.push_back(op);
      } else if (left != mismatch && right != mismatch) {
        coerce_r.push_back(op);
      }
    };
    auto binInsert = [](const self::OperatorDef *o, auto lhs, auto rhs) {
      auto result = std::make_unique<self::FunctionCall>(*o);
      if (auto *var = dynamic_cast<self::VarDeclaration *>(lhs->get())) {
        var->value = rhs->get();
      }
      result->lhs = std::move(*lhs);
      result->rhs = std::move(*rhs);
      return result;
    };
    auto bin_coerce = [](const self::OperatorDef &fun, auto &lhs, auto &rhs) {
      if (fun.internal == self::detail::assign ||
          fun.internal == self::detail::store) {
        auto &var = dynamic_cast<self::VarDeclaration &>(*lhs);
        var.value = rhs.get();
      }
      coerceType(lhs.get(), fun.lhs->getDecltype());
      coerceType(rhs.get(), fun.rhs->getDecltype());
    };
    // left-right associative pass
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      processFunction<self::OperatorDef>(
          it, true, [](auto &lhs, auto &rhs) { --lhs, ++rhs; }, binCondition,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs);
            it = tree.erase(lhs);
          },
          binInsert, bin_coerce, local);
    }

    // right-left associative pass
    for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
      processFunction<self::OperatorDef>(
          it, false, [](auto &lhs, auto &rhs) { ++lhs, --rhs; }, binCondition,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs.base());
            it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
          },
          binInsert, bin_coerce, local);
    }
    errReport(tree.size() == 1, "tree is not fully resolved");
    return self::foldExpr(std::move(tree.back()), local, global).first;
  }

  self::ExprPtr parseSymbol(self::ExprPtr &base,
                                  self::SymbolMap &local,
                                  self::SymbolMap &global) {
    if (auto *maybe = dynamic_cast<self::UnevaluatedExpression *>(base.get());
        maybe && !maybe->isComplete()) {
      auto t = maybe->getToken();
      if (auto [is_int, number] = isInt(t); is_int) {
        return std::make_unique<self::IntLit>(number, c);

      } else if (auto [result, ch] = isChar(t); result) {
        return std::make_unique<self::CharLit>(ch, c);
      } else if (auto [result, str] = isStr(t); result) {
        return std::make_unique<self::StringLit>(str, c);
      } else if (auto [result, boolean] = isBool(t); result) {
        return std::make_unique<self::BoolLit>(boolean, c);
      } else if (self::BuiltinTypeLit::contains(t, c)) {
        return std::make_unique<self::BuiltinTypeLit>(
            self::BuiltinTypeLit::get(t, c));
      } else if (auto varname = self::VarDeclaration::mangle(t);
                 local.contains(varname)) {
        errReport(local.count(varname) == 1,
                  "ODR var declaration rule violated");
        return std::make_unique<self::VarDeref>(
            dynamic_cast<const self::VarDeclaration &>(
                local.find(varname)->second.get()));
      } else if (global.contains(varname)) {
        errReport(global.count(varname) == 1,
                  "ODR var declaration rule violated");
        return std::make_unique<self::VarDeref>(
            dynamic_cast<const self::VarDeclaration &>(
                global.find(varname)->second.get()));
      }
    }
    return std::move(base);
  }
  using callback = std::function<void(self::ExprTree &)>;
  using conditional = std::function<bool(self::TokenView)>;

  constexpr static auto isEndl = [](self::TokenView t) -> bool {
    return t == self::reserved::endl;
  };

  self::ExprPtr parseExpr(TokenIt &t, self::SymbolMap &context,
                                callback start = nullptr,
                                conditional endExpr = isEndl) {
    self::ExprTree tree;
    if (start) {
      start(tree);
    }
    while (!endExpr(*t)) {
      if (*t == self::reserved::struct_t) {
        tree.push_back(parseStruct(++t));
      } else if (*t == self::reserved::var_t) {
      } else {
        tree.push_back(std::make_unique<self::UnevaluatedExpression>(*t++));
      }
    }
    return evaluateTree(tree, context);
  }

  self::ExprPtr parseVar(TokenIt &t, self::TokenView name,
                               self::SymbolMap &context) {
    using namespace self::reserved;
    const auto guard = [this](auto t) {
      if (!notReserved(t)) {
        std::stringstream err;
        err << "Token " << t << " is reserved";
        errReport(false, err.str());
      }
    };
    guard(name);
    auto curr = std::make_unique<self::VarDeclaration>(name);
    if (*t == ":") {
      ++t;
      auto expr = parseExpr(t, context);
      auto type = self::getLiteralType(*expr);
      curr->type_ref = {&type.ptr, type.is_ref};
      context.insert({curr->getName(), std::cref(*curr)});
      return curr;
    } else {
      errReport(notReserved(*t), "non-reserved Token expected in expression");
      context.insert({curr->getName(), std::cref(*curr)});
      return parseExpr(t, context, [&](self::ExprTree &tree) {
        tree.push_back(std::move(curr));
      });
    }
  }

  self::ExprPtr parseFun(TokenIt &t, self::TokenView name,
                               self::SymbolMap &context) {
    auto curr = std::make_unique<self::FunctionDef>(name);
    self::SymbolMap local;
    errReport(*t++ == "(", "\"(\" expected");
    while (*t != ")") {
      errReport(notReserved(*t),
                "reserved Token cannot be used as parameter name");
      curr->arguments.emplace_back(
          std::make_unique<self::VarDeclaration>(*t++));
      errReport(*t++ == ":", "\":\" expected here");
      constexpr auto commaOrParen = [](self::TokenView t) {
        return t == "," || t == ")";
      };
      auto e = parseExpr(t, context, nullptr, commaOrParen);
      auto type = self::getLiteralType(*e);
      curr->arguments.back()->type_ref = {&type.ptr, type.is_ref};
      errReport(*t == ")" || *t == ",", "\")\" or \",\" expected");
      if (*t == ")")
        break;
      else
        ++t;
    }
    if (*++t == "->") {
      ++t;
      constexpr auto commaOrBracket = [](self::TokenView t) {
        return t == self::reserved::endl || t == "{";
      };
      auto e = parseExpr(t, context, nullptr, commaOrBracket);
      auto type = self::getLiteralType(*e);
      curr->return_type = {&type.ptr, type.is_ref};
      curr->body_defined = false;
    }
    if (*t == "{") {
      while (*t != "}") {
        using namespace self::reserved;
        if (*t == var_t) {
          auto name = *++t;
          curr->body.push_back(parseVar(++t, name, local));
        } else if (*t++ == return_t) {
          if (*t != endl) {
            curr->body.push_back(
                std::make_unique<self::Ret>(parseExpr(t, local)));
          } else {
            curr->body.push_back(std::make_unique<self::Ret>());
          }
        } else if (notReserved(*t)) {
          curr->body.push_back(parseExpr(t, local));
        }
      }
      curr->body_defined = true;
    }
    ++t;
    context.insert({curr->name, *curr});
    return curr;
  }

  self::ExprPtr parseStruct(TokenIt &t) {
    self::SymbolMap context;
    static unsigned int id = 0;
    auto identity = std::string("struct");
    identity.append(std::to_string(id++));
    errReport(*t == "{" || *t == "(", "Expected a \"{\" or \"(\"");
    if (*t == "(") {
      ++t;
      if (*t == ")") {
        ++t;
        return std::make_unique<self::OpaqueLit>(self::OpaqueStruct(id, 0), c);
      } else {
        auto [success, size] = isInt(*t++);
        errReport(success, "Expected integer or \")\"");
        return std::make_unique<self::OpaqueLit>(self::OpaqueStruct(id, size),
                                                 c);
      }
    } else {
      auto result = self::StructDef();
      while (*t != "}") {
        if (*(++t) == "var") {
          auto name = *++t;
          result.body.push_back(parseVar(++t, name, context));
        } else if (*t == "fun") {
          auto name = *++t;
          errReport(notReserved(name), "function name is reserved");
          result.body.push_back(parseFun(++t, name, context));
        } else {
          errReport(*t != self::reserved::endl,
                    "expected a function or variable declaration.");
        }
      }
      ++t;
      result.identity = id;
      return std::make_unique<self::StructLit>(std::move(result), c);
    }
  }

  self::ExprTree process(TokenIt t) {
    self::ExprTree syntax_tree;
    using namespace self::reserved;
    while (!t.end()) {
      if (*t == var_t) {
        auto name = *++t;
        syntax_tree.push_back(parseVar(++t, name, global));
      } else if (*t == fun_t) {
        auto name = *++t;
        errReport(notReserved(name), "function name is reserved");
        syntax_tree.push_back(parseFun(++t, name, global));
      } else if (notReserved(*t)) {
        syntax_tree.push_back(parseExpr(t, global));
      } else {
        errReport(*t++ == endl, "invalid expression");
      }
    }
    return syntax_tree;
  }

  GlobalParser(self::Context &c) : c(c) {
    const auto symbolInserter = [this](const self::ExprBase &expr) {
      global.insert({expr.getName(), std::cref(expr)});
    };
    symbolInserter(c.i64_assignment);
    symbolInserter(c.addi);
    symbolInserter(c.subi);
    symbolInserter(c.muli);
    symbolInserter(c.divi);
    symbolInserter(c.struct_assignment);
  }
};
} // namespace

namespace self {
ExprTree parse(TokenVec in, Context &c) {
  auto parser = GlobalParser(c);
  return parser.process(TokenIt{in});
}
} // namespace self