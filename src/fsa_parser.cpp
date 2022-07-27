#include <algorithm>
#include <bits/iterator_concepts.h>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "ast/Index.hpp"
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
#include "error_handling.hpp"
#include "ffi_parse.hpp"
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

void escape(std::vector<unsigned char> &literal) {
  for (auto c = literal.begin(); c != literal.end(); ++c) {
    if (*c == '\\') {
      auto mark = c;
      ++c;
      if (std::isdigit(*c)) {
        char *end;
        auto value = std::strtol(reinterpret_cast<const char *>(&*c), &end, 10);
        if (value > std::numeric_limits<unsigned char>::max()) {
          throw std::runtime_error("escaped char value is too big.");
        }
        // this is a stupid workaround but I guess it'll work
        auto distance = std::distance(reinterpret_cast<char *>(&*c), end);
        c = --literal.erase(c, c + 1 + distance);
        *mark = value;
      } else if (*c == 'n') {
        *mark = '\n';
        c = --literal.erase(c);
      }
    }
  }
  literal.erase(literal.begin() + literal.size() - 1);
  literal.erase(literal.begin());
}

std::vector<unsigned char> convertString(std::string_view s) {
  std::vector<unsigned char> result;
  result.reserve(s.size());
  std::for_each(s.cbegin(), s.cend(), [&](char c) { result.push_back(c); });
  return result;
}

std::pair<bool, std::vector<unsigned char>> isStr(self::TokenView t) {
  if (t.front() != '\'' && t.front() != '\"')
    return {false, {}};
  if (!t.ends_with('\'') && !t.ends_with('\"'))
    return {false, {}};
  auto literal = convertString(t);
  escape(literal);
  return {true, literal};
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

struct TokenIt {
  TokenIt(self::LexedFileRef &where) : where(where) {}
  self::LexedFileRef &where;
  size_t pos = 0;
  self::TokenView operator*() { return where.tokens.at(pos); }
  TokenIt &operator++() {
    if (pos >= where.tokens.size()) {
      throw std::runtime_error("out of bounds");
    }
    ++pos;
    return *this;
  }
  TokenIt &operator--() {
    if (pos >= where.tokens.size()) {
      throw std::runtime_error("out of bounds");
    }
    ++pos;
    return *this;
  }
  TokenIt operator++(int) {
    TokenIt tmp = *this;
    this->operator++();
    return tmp;
  }
  TokenIt next() const noexcept { return TokenIt{where, pos + 1}; }
  TokenIt prev() const noexcept { return TokenIt{where, pos - 1}; }
  bool end() const noexcept { return pos == where.tokens.size(); }
  size_t nextLineLength() const {
    TokenIt tmp = *this;
    size_t length = 0;
    while (*tmp != self::reserved::endl) {
      ++tmp;
      ++length;
    }
    return length;
  }
  size_t prevLineLength() const {
    TokenIt tmp = *this;
    size_t length = 0;
    while (*tmp != self::reserved::endl) {
      --tmp;
      ++length;
    }
    return length;
  }
  auto getCol() const {
    // bubble search may be naive
    // but probably works better for
    // small sized n's
    unsigned prev = 0;
    for (auto length : where.line_pos) {
      if (length > pos) {
        return pos - prev;
      }
      prev = length;
    }
    throw std::logic_error("Failed to get line column");
  }

  auto getRow() const {
    size_t count = 0;
    for (auto length : where.line_pos) {
      if (length > pos) {
        return count;
      }
      ++count;
    }
    throw std::logic_error("Failed to get line column");
  }

private:
  TokenIt(self::LexedFileRef &where, size_t pos) : where(where), pos(pos) {}
};

struct GlobalParser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  using TypeList = std::unordered_map<self::TokenView, self::TypeRef>;
  self::Context &c;
  self::ErrorList &err;
  // todo: add proper compile error reporting later.
  auto errReport(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }

  static void processSubtrees(self::ExprTree &tree, auto begin, auto end) {
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

  void processTuples(self::ExprTree &tree, self::Index &context) {
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
          tuple->members.emplace_back(parseSymbol(branch.back(), context));
        }
        size = 0;
      }
    }
    if (!tuple->members.empty()) {
      if (tree.size() > 1) {
        tuple->members.emplace_back(evaluateTree(tree, context));
      } else {
        tuple->members.emplace_back(parseSymbol(tree.back(), context));
        tree.pop_back();
      }
      tuple->members.shrink_to_fit();
      tree.push_back(std::move(tuple));
    }
  }

  enum struct coerce_result { match, coerce, mismatch };
  coerce_result needCoerce(const self::ExprBase *e, self::TypePtr type) {
    using enum coerce_result;
    if (const auto *var = dynamic_cast<const self::VarDeclaration *>(e)) {
      if (!var->getDecltype().ptr) {
        return coerce;
      } else if (var->getDecltype() == type) {
        return match;
      }
    } else if (const auto *uneval =
                   dynamic_cast<const self::UnevaluatedExpression *>(e)) {
      return coerce;
    }
    auto etype = e->getType();
    if (etype.ptr == type.ptr) {
      if (etype.depth == type.depth)
        return match;
      else if (etype.depth - 1 == type.depth)
        return coerce;
    }
    return mismatch;
  }

  static bool coerceType(self::ExprBase *e, self::TypePtr type) {
    if (auto *var = dynamic_cast<self::VarDeclaration *>(e)) {
      if (!var->type_ref.ptr) {
        var->type_ref = type;
        if (type.depth == 0)
          throw std::runtime_error("var coercion depth should not be 0");
        --var->type_ref.depth;
        if (var->type_ref.depth == 0)
          var->type_ref.is_ref = self::RefTypes::value;
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
                       self::Index &context) {
    if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      auto candidates =
          self::pair_range(context.equal_range(T::mangle(t->getToken())));
      std::vector<const T *> no_coerce;
      std::vector<const T *> coerced;
      for (auto &[_, fun] : candidates) {
        const auto &op = dynamic_cast<const T &>(fun.get());
        if (not_a_member ^ op.member) {
          cond(&op, lhs, rhs, no_coerce, coerced);
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
  }

  self::ExprPtr evaluateTree(self::ExprTree &tree, self::Index &local) {
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
    processTuples(tree, local);
    for (auto &ptr : tree) {
      ptr = parseSymbol(ptr, local);
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
              return true;
            }
            return false;
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
      auto left = needCoerce(lhs->get(), op->lhs->getDecltype());
      auto right = needCoerce(rhs->get(), op->rhs->getDecltype());
      if (left == match && right == match) {
        no_coerce.push_back(op);
        return true;
      } else if (left != mismatch && right != mismatch) {
        coerce_r.push_back(op);
        return true;
      }
      return false;
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
    return self::foldExpr(std::move(tree.back()), local).first;
  }

  self::ExprPtr parseSymbol(self::ExprPtr &base, self::Index &local) {
    if (auto *maybe = dynamic_cast<self::UnevaluatedExpression *>(base.get());
        maybe && !maybe->isComplete()) {
      auto t = maybe->getToken();
      if (auto [is_int, number] = isInt(t); is_int) {
        return std::make_unique<self::IntLit>(number, c);
      } else if (auto [result, str] = isStr(t); result) {
        return std::make_unique<self::StringLit>(str, c);
      } else if (auto [result, boolean] = isBool(t); result) {
        return std::make_unique<self::BoolLit>(boolean, c);
      } else if (self::BuiltinTypeLit::contains(t, c)) {
        return std::make_unique<self::BuiltinTypeLit>(
            self::BuiltinTypeLit::get(t, c));
      } else if (auto varname = self::VarDeclaration::mangle(t);
                 local.contains(varname)) {
        errReport(local.isUnique(varname), "ODR var declaration rule violated");
        return std::make_unique<self::VarDeref>(
            dynamic_cast<const self::VarDeclaration &>(
                local.find(varname)->get()));
      }
    }
    return std::move(base);
  }
  using callback = std::function<void(self::ExprTree &)>;

  constexpr static auto default_end = [](self::TokenView t) -> bool {
    return t == self::reserved::endl || t == "}";
  };

  self::ExprPtr
  parseExpr(TokenIt &t, self::Index &context, callback start = nullptr,
            std::function<bool(self::TokenView)> endExpr = default_end) {
    self::ExprTree tree;
    if (start) {
      start(tree);
    }
    while (!endExpr(*t)) {
      if (*t == self::reserved::struct_t) {
        tree.push_back(parseStruct(++t, context));
      } else if (*t == self::reserved::var_t) {
        auto name = *++t;
        tree.push_back(parseVar(++t, name, context));
      } else {
        tree.push_back(std::make_unique<self::UnevaluatedExpression>(*t++));
      }
    }
    return evaluateTree(tree, context);
  }

  self::ExprPtr parseVar(TokenIt &t, self::TokenView name,
                         self::Index &context) {
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
      context.insert({curr->getName(), std::ref(*curr)});
      return curr;
    } else {
      errReport(notReserved(*t), "non-reserved Token expected in expression");
      context.insert({curr->getName(), std::ref(*curr)});
      return parseExpr(t, context, [&](self::ExprTree &tree) {
        tree.push_back(std::move(curr));
      });
    }
  }

  void consumeNullExpr(TokenIt &t) {
    if (*t == self::reserved::endl)
      ++t;
  }

  std::unique_ptr<self::Block> forceBlock(TokenIt &t, self::Block &parent) {
    // this exists to silence a warning about side-effects in typeid expressions
    if (*t == "{") {
      ++t;
      return std::make_unique<self::Block>(parseBlock(t, parent.contexts));
    }
    auto results = std::make_unique<self::Block>(parent.contexts);
    results->push_back(parseExpr(t, results->contexts));
    return results;
  };

  void parseIf(TokenIt &t, self::Block &body) {
    ++t;
    auto if_statement = std::make_unique<self::If>();

    if_statement->condition =
        parseExpr(t, body.contexts, nullptr, [](self::TokenView t) -> bool {
          return t == ";" || t == "{";
        });
    errReport(if_statement->condition->getType().ptr == &c.bool_t,
              "if condition expression is supposed to be boolean.");
    consumeNullExpr(t);
    if_statement->block = forceBlock(t, body);
    if (*t.next() == self::reserved::else_t) {
      ++ ++t;
      consumeNullExpr(t);
      if_statement->else_block = forceBlock(t, body);
    }
    body.push_back(std::move(if_statement));
  }

  void parseWhile(TokenIt &t, self::Block &body) {
    self::ExprPtr condition;
    if (*t == self::reserved::while_t) {
      ++t;
      condition =
          parseExpr(t, body.contexts, nullptr, [](self::TokenView t) -> bool {
            return t == ";" || t == "{";
          });
      if (*t == ";")
        ++t;
    } else {
      // assumes *t == "do"
      ++t;
    }
    auto block = forceBlock(t, body);
    bool is_do = false;
    if (condition == nullptr) {
      is_do = true;
      ++t;
      errReport(*t == self::reserved::while_t, "expected a 'while' after a do");
      ++t;
      condition = parseExpr(t, body.contexts);
      ++t;
    }
    body.push_back(std::make_unique<self::While>(std::move(block),
                                                 std::move(condition), is_do));
  }

  self::Block parseBlock(TokenIt &t, self::Index &parent) {
    auto body = self::Block(parent);
    while (*t != "}") {
      using namespace self::reserved;
      if (*t == var_t) {
        auto name = *++t;
        body.push_back(parseVar(++t, name, body.contexts));
      } else if (*t == return_t) {
        ++t;
        if (*t != endl) {
          body.push_back(
              std::make_unique<self::Ret>(parseExpr(t, body.contexts)));
        } else {
          body.push_back(std::make_unique<self::Ret>());
        }
      } else if (*t == if_t) {
        parseIf(t, body);
      } else if (*t == while_t || *t == do_t) {
        parseWhile(t, body);
      } else if (notReserved(*t)) {
        body.push_back(parseExpr(t, body.contexts));
      } else {
        ++t;
      }
    }
    ++t;
    return body;
  }

  self::ExprPtr parseFun(TokenIt &t, self::TokenView name,
                         self::Index &parent) {
    auto curr = std::make_unique<self::FunctionDef>(name, parent);
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
      auto e = parseExpr(t, parent, nullptr, commaOrParen);
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
      auto e = parseExpr(t, parent, nullptr, commaOrBracket);
      auto type = self::getLiteralType(*e);
      curr->return_type = {&type.ptr, type.is_ref};
      curr->body_defined = false;
    }
    if (*t == "{") {
      ++t;
      curr->body.emplace(parseBlock(t, parent));
      curr->body_defined = true;
    }
    parent.insert({curr->name, *curr});
    return curr;
  }

  self::ExprPtr parseStruct(TokenIt &t, self::Index &parent) {
    static unsigned int id = 0;
    auto identity = std::string("struct");
    identity.append(std::to_string(id++));
    errReport(*t == "{" || *t == "(", "Expected a \"{\" or \"(\"");
    if (*t == "(") {
      ++t;
      if (*t == ")") {
        ++t;
        return std::make_unique<self::StructDef>(0, parent);
      } else {
        auto [success, size] = isInt(*t++);
        errReport(success, "Expected integer or \")\"");
        return std::make_unique<self::StructDef>(size, parent);
      }
    } else {
      auto result = self::StructDef(parent);
      ++t;
      while (*t != "}") {
        if (*t == "var") {
          auto name = *++t;
          result.body.push_back(parseVar(++t, name, result.context));
        } else if (*t == "fun") {
          auto name = *++t;
          errReport(notReserved(name), "function name is reserved");
          result.body.push_back(parseFun(++t, name, result.context));
        } else {
          errReport(*t == self::reserved::endl,
                    "expected a function or variable declaration.");
          ++t;
        }
      }
      ++t;
      result.identity = id;
      return std::make_unique<self::StructLit>(std::move(result), c);
    }
  }

  auto fileOpen(std::string_view p) {
    auto path = std::string(p);
    auto file = std::fstream(path);
    errReport(file.good(), "file has failed to open");
    std::stringstream result;
    result << file.rdbuf();
    return result.str();
  }

  auto parseStrLit(TokenIt &t, std::string_view err) {
    auto [a, b] = isStr(*t);
    errReport(a, err);
    std::string unconvert;
    unconvert.reserve(b.size());
    std::for_each(b.begin(), b.end(),
                  [&](unsigned char c) { unconvert.push_back(c); });
    return unconvert;
  }

  auto processImport(TokenIt &t, self::ExprTree &syntax_tree,
                     self::Index &global) {
    auto path = parseStrLit(t, "Import path must be a string literal");
    ++t;
    auto file = fileOpen(path);
    auto tokens = self::detail::parseToken(self::detail::preprocess(file));
    process(TokenIt{tokens}, syntax_tree, global);
  }

  auto processExtern(TokenIt &t, self::ExprTree &syntax_tree) {
    auto spec = parseStrLit(t, "extern specification must be a string literal");
    if (spec == "C") {
      ++t;
      errReport(*t++ == self::reserved::import_t,
                "expected import after extern specification");
      auto path = parseStrLit(t, "Import path must be a string literal");
      ++t;
      self::parseFFI(syntax_tree, c.root, c, path, "-O2");
    } else {
      errReport(false, "unknown extern specification");
    }
  }

  void process(TokenIt t, self::ExprTree &syntax_tree, self::Index &global) {
    using namespace self::reserved;
  retry:
    try {
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
        } else if (*t == import_t) {
          processImport(++t, syntax_tree, global);
        } else if (*t == "extern") {
          processExtern(++t, syntax_tree);
        } else {
          errReport(*t++ == endl, "invalid expression");
        }
      }
    } catch (std::runtime_error e) {
      err.errors.push_back(self::Error{t.getCol(), t.getRow(), e.what()});
      while (*t != ";")
        ++t;
      goto retry;
    }
  }

  self::ExprTree process(TokenIt t, self::Index &i) {
    self::ExprTree syntax_tree;
    process(t, syntax_tree, i);
    return syntax_tree;
  }

  GlobalParser(self::Context &c, self::ErrorList &e) : c(c), err(e) {}
};
} // namespace

namespace self {
Module parse(LexedFileRef &in, Context &c) {
  ErrorList e;
  auto parser = GlobalParser(c, e);
  auto root = Index(c.root);
  auto ast = parser.process(TokenIt{in}, root);
  return Module(std::move(root), std::move(ast), std::move(e));
}
} // namespace self