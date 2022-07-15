#include <algorithm>
#include <cctype>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
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
  using type_list = std::unordered_map<self::TokenView, self::TypeRef>;
  type_list types;
  using expr_ref = std::reference_wrapper<const self::Expression>;
  using symbol_map = std::unordered_multimap<self::TokenView, expr_ref>;
  symbol_map symbols;
  std::vector<std::string> curr_scope;
  // todo: add proper compile error reporting later.
  auto errReport(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }

  static void processSubtrees(self::ExpressionTree &tree, auto begin, auto end) {
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
              auto subtree = std::make_unique<self::ExpressionTree>();
              subtree->reserve(std::distance(open + 1, close));
              std::for_each(open + 1, close, [&](self::ExpressionPtr &e) {
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

  static void processSubtrees(self::ExpressionTree &tree) {
    return processSubtrees(tree, tree.begin(), tree.end());
  }

  void processTuples(self::ExpressionTree &tree) {
    size_t size = 0;
    auto tuple = std::make_unique<self::Tuple>();
    auto mark = tree.begin();
    for (auto it = tree.begin(); it != tree.end(); ++it, ++size) {
      if (auto *comma = dynamic_cast<self::UnevaluatedExpression *>(it->get());
          comma && comma->getToken() == ",") {
        self::ExpressionTree branch;
        branch.reserve(size);
        std::for_each(mark, it, [&](self::ExpressionPtr &ptr) {
          branch.push_back(std::move(ptr));
        });
        mark = it = tree.erase(mark, it + 1);
        if (branch.size() > 1) {
          tuple->members.emplace_back(evaluateTree(branch));
        } else {
          tuple->members.emplace_back(parseSymbol(branch.back()));
        }
        size = 0;
      }
    }
    if (!tuple->members.empty()) {
      if (tree.size() > 1) {
        tuple->members.emplace_back(evaluateTree(tree));
      } else {
        tuple->members.emplace_back(parseSymbol(tree.back()));
        tree.pop_back();
      }
      tuple->members.shrink_to_fit();
      tree.push_back(std::move(tuple));
    }
  }

  enum struct coerce_result { match, coerce, mismatch };
  coerce_result need_coerce(const self::Expression *e, self::TypePtr type) {
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

  static bool coerceType(self::Expression *e, self::TypePtr type) {
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

  static size_t tuple_count(self::Expression *e) {
    if (auto *tuple = dynamic_cast<self::Tuple *>(e))
      return tuple->members.size();
    return 1;
  }

  static void for_tuple(self::Expression *e, auto unary) {
    if (auto *tuple = dynamic_cast<self::Tuple *>(e)) {
      for (auto &a : tuple->members) {
        unary(*a);
      }
    }
    unary(*e);
  }

  static void for_tuple(self::Expression *left, auto gen, auto binary) {
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
                auto cleanup, auto insert, auto coerce) {
    if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
      auto candidates = self::pair_range(symbols.equal_range(t->getToken()));
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      std::vector<const T *> no_coerce;
      std::vector<const T *> corced;
      auto count = std::distance(candidates.begin(), candidates.end());
      for (auto &[_, fun] : candidates) {
        if (const auto *op = dynamic_cast<const T *>(&fun.get());
            op && (not_a_member ^ op->member)) {
          cond(op, lhs, rhs, no_coerce, corced);
        }
      }
      if (!no_coerce.empty()) {
        errReport(no_coerce.size() == 1, "ambiguous operator call");
        auto result = insert(no_coerce.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      } else if (!corced.empty()) {
        errReport(corced.size() == 1, "ambiguous operator call");
        coerce(*corced.back(), *lhs, *rhs);
        auto result = insert(corced.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      }
    }
  };

  self::ExpressionPtr evaluateTree(self::ExpressionTree &tree) {
    if (tree.empty()) {
      return std::make_unique<self::Tuple>();
    }
    processSubtrees(tree);
    for (auto &ptr : tree) {
      if (auto *uneval = dynamic_cast<self::ExpressionTree *>(ptr.get())) {
        ptr = evaluateTree(*uneval);
      }
    }
    processTuples(tree);
    for (auto &ptr : tree) {
      ptr = parseSymbol(ptr);
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
              result->RHS =
                  std::make_unique<self::arg_pack>(std::move(*tuple));
            } else {
              result->RHS = std::move(*rhs);
            }
            return result;
          }, [](const self::FunctionDef& fun, auto& lhs, auto& rhs){
            int i = 0;
            for_tuple(rhs.get(), [&]{
              return fun.arguments.at(i++)->getDecltype();},
              [](self::Expression& e, auto type){coerceType(&e, type);});
          });
    }
    #pragma clang diagnostic pop // clang-format on


    auto binCondition = [&](auto *op, auto lhs, auto rhs, auto &no_coerce,
                        auto &coerce_r) {
      using enum coerce_result;
      auto left = need_coerce(lhs->get(), op->LHS->getDecltype());
      auto right = need_coerce(rhs->get(), op->RHS->getDecltype());
      if (left == match && right == match) {
        no_coerce.push_back(op);
      } else if (left != mismatch && right != mismatch) {
        coerce_r.push_back(op);
      }
    };
    auto binInsert = [](const self::OperatorDef *o, auto lhs, auto rhs) {
      auto result = std::make_unique<self::FunctionCall>(*o);
      result->LHS = std::move(*lhs);
      result->RHS = std::move(*rhs);
      return result;
    };
    auto bin_coerce = [](const self::OperatorDef &fun, auto &lhs, auto &rhs) {
      coerceType(lhs.get(), fun.LHS->getDecltype());
      coerceType(rhs.get(), fun.RHS->getDecltype());
    };
    // left-right associative pass
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      processFunction<self::OperatorDef>(
          it, true, [](auto &lhs, auto &rhs) { --lhs, ++rhs; }, binCondition,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs);
            it = tree.erase(lhs);
          },
          binInsert, bin_coerce);
    }

    // right-left associative pass
    for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
      processFunction<self::OperatorDef>(
          it, false, [](auto &lhs, auto &rhs) { ++lhs, --rhs; }, binCondition,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs.base());
            it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
          },
          binInsert, bin_coerce);
    }

    errReport(tree.size() == 1, "tree is not fully resolved");
    auto last = self::ExpressionPtr(std::move(tree.back()));
    tree.pop_back();
    return last;
  }

  self::ExpressionPtr parseSymbol(self::ExpressionPtr &base) {
    if (auto *maybe = dynamic_cast<self::UnevaluatedExpression *>(base.get());
        maybe && !maybe->isComplete()) {
      auto t = maybe->getToken();
      if (auto [is_int, number] = isInt(t); is_int) {
        return std::make_unique<self::IntLit>(number);

      } else if (auto [result, c] = isChar(t); result) {
        return std::make_unique<self::CharLit>(c);

      } else if (auto [result, str] = isStr(t); result) {
        std::vector<unsigned char> value;
        value.reserve(str.size() - 1);
        std::copy(str.begin(), str.end() - 1, std::back_inserter(value));
        return std::make_unique<self::StringLit>(value);
      }
    }
    return std::move(base);
  }
  using callback = std::function<void(self::ExpressionTree &)>;

  self::ExpressionPtr parseExpr(TokenIt &t, callback start = nullptr) {
    auto tree = std::make_unique<self::ExpressionTree>();
    if (start) {
      start(*tree);
    }
    while (*t != self::reserved::endl) {
      if (*t == self::reserved::struct_t) {
        tree->push_back(parseStruct(++t));
      } else if (*t == self::reserved::var_t) {
      } else {
        tree->push_back(std::make_unique<self::UnevaluatedExpression>(*t++));
      }
    }
    auto result = evaluateTree(*tree);
    return result;
  }

  self::ExpressionPtr parseVar(TokenIt &t, self::TokenView name) {
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
      // TODO: deal with multiple candidates
      auto candidates = types.find(*t++);
      errReport(candidates != types.end(), "type not found");
      curr->type_ref.ptr = &candidates->second.ptr;
      errReport(*t == endl, "end of line expected");
      return curr;
    } else {
      errReport(notReserved(*t), "non-reserved Token expected in expression");
      return parseExpr(t, [&](self::ExpressionTree &tree) {
        tree.push_back(std::move(curr));
      });
    }
  }

  self::ExpressionPtr parseFun(TokenIt &t, self::TokenView name) {
    auto g = self::scope_guard([&]() { curr_scope.emplace_back(name); },
                               [&]() noexcept { curr_scope.pop_back(); });
    auto curr = std::make_unique<self::FunctionDef>(name);
    errReport(*t++ == "(", "\"(\" expected");
    while (*t != ")") {
      errReport(notReserved(*t),
                "reserved Token cannot be used as parameter name");
      curr->arguments.emplace_back(
          std::make_unique<self::VarDeclaration>(*t++));
      errReport(*t++ == ":", "\":\" expected here");
      // todo make this an expression instead of hardcoded
      auto candidates = types.find(*t++);
      errReport(candidates != types.end(), "no types found");
      curr->arguments.back()->type_ref.ptr = &candidates->second.ptr;
      errReport(*t == ")" || *t == ",", "\")\" or \",\" expected");
      if (*t == ")")
        break;
      else
        ++t;
    }
    if (*++t == "->") {
      ++t;
      auto candidates = types.find(*t++);
      errReport(candidates != types.end(), "type not found");
      curr->return_type.ptr = &candidates->second.ptr;
      curr->body_defined = false;
    }
    if (*t == "{") {
      while (*t != "}") {
        using namespace self::reserved;
        if (*t == var_t) {
          auto name = *++t;
          curr->body.push_back(parseVar(++t, name));
        } else if (*t++ == return_t) {
          if (*t != endl) {
            curr->body.push_back(std::make_unique<self::Ret>(parseExpr(t)));
          } else {
            curr->body.push_back(std::make_unique<self::Ret>());
          }
        } else if (notReserved(*t)) {
          curr->body.push_back(parseExpr(t));
        }
      }
      curr->body_defined = true;
    }
    ++t;
    symbols.insert({curr->name, *curr});
    return curr;
  }

  self::ExpressionPtr parseStruct(TokenIt &t) {
    static unsigned int id = 0;
    auto identity = std::string("struct");
    identity.append(std::to_string(id++));
    auto g = self::scope_guard([&]() { curr_scope.emplace_back(identity); },
                               [&]() noexcept { curr_scope.pop_back(); });
    errReport(*t == "{" || *t == "(", "Expected a \"{\" or \"(\"");
    if (*t == "(") {
      ++t;
      if (*t == ")") {
        ++t;
        auto a = std::make_unique<self::OpaqueLit>(self::OpaqueStruct(id, 0));
        symbols.insert({std::string_view(curr_scope.back()), *a});
        return a;
      } else {
        auto [success, size] = isInt(*t++);
        errReport(success, "Expected integer or \")\"");
        auto a =
            std::make_unique<self::OpaqueLit>(self::OpaqueStruct(id, size));
        symbols.insert({std::string_view(curr_scope.back()), *a});
        return a;
      }
    } else {
      auto result = self::StructDef();
      while (*t != "}") {
        if (*(++t) == "var") {
          auto name = *++t;
          result.body.push_back(parseVar(++t, name));
        } else if (*t == "fun") {
          auto name = *++t;
          errReport(notReserved(name), "function name is reserved");
          result.body.push_back(parseFun(t, name));
        } else {
          errReport(*t != self::reserved::endl,
                    "expected a function or variable declaration.");
        }
      }
      ++t;
      result.identity = id;
      auto a = std::make_unique<self::StructLit>(std::move(result));
      symbols.insert({std::string_view(curr_scope.back()), *a});
      return a;
    }
  }

  self::ExpressionTree process(TokenIt t) {
    self::ExpressionTree syntax_tree;
    using namespace self::reserved;
    while (!t.end()) {
      if (*t == var_t) {
        auto name = *++t;
        syntax_tree.emplace_back(parseVar(++t, name));
      } else if (*t == fun_t) {
        auto name = *++t;
        errReport(notReserved(name), "function name is reserved");
        syntax_tree.emplace_back(parseFun(++t, name));
      } else if (notReserved(*t)) {
        syntax_tree.emplace_back(parseExpr(t));
      } else {
        errReport(*t++ == endl, "invalid expression");
      }
    }
    return syntax_tree;
  }

  GlobalParser() {
    const auto typeInserter = [this](const self::Type &var) {
      types.insert({var.getTypename(), var});
    };
    typeInserter(self::type_inst);
    typeInserter(self::i64_t.value);
    typeInserter(self::char_t.value);
    const auto symbolInserter = [this](const self::Expression &expr) {
      symbols.insert({expr.getName(), std::cref(expr)});
    };
    symbolInserter(self::i64_assignment);
    symbolInserter(self::addi);
    symbolInserter(self::subi);
    symbolInserter(self::muli);
    symbolInserter(self::divi);
    symbolInserter(self::struct_assignment);
  }
};
} // namespace

namespace self {
ExpressionTree parse(TokenVec in) {
  GlobalParser parser;
  return parser.process(TokenIt{in});
}
} // namespace self