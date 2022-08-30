#include <algorithm>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
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
#include "ast/visitor.hpp"
#include "ffi_parse.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "utility/pair_range.hpp"
#include "utility/scope_guard.hpp"

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

std::optional<std::vector<unsigned char>> isStr(self::TokenView t) {
  if (t.front() != '\'' && t.front() != '\"')
    return std::nullopt;
  if (!t.ends_with('\'') && !t.ends_with('\"'))
    return std::nullopt;
  auto literal = convertString(t);
  escape(literal);
  return literal;
}

std::optional<bool> isBool(self::TokenView t) {
  if (t == "true")
    return true;
  else if (t == "false")
    return false;
  else
    return std::nullopt;
}

constexpr auto notReserved = [](auto t) {
  return !self::reserved::isKeyword(t) && !self::reserved::isGrammar(t);
};

struct TokenIt {
  TokenIt(self::LexedFileRef &where) : where(where), col(1), line(1) {}
  self::LexedFileRef &where;
  size_t pos = 0;
  size_t col = 0, line = 0;
  self::Pos coord() const noexcept { return {col, line}; }
  self::TokenView operator*() { return where.tokens.at(pos); }
  TokenIt &operator++() {
    if (pos > where.tokens.size()) {
      throw std::runtime_error("out of bounds");
    }
    ++pos;
    if (!end() && self::reserved::isEndl(**this)) {
      col = 1;
      ++line;
    } else {
      ++col;
    }
    return *this;
  }
  TokenIt &operator--() {
    if (pos >= where.tokens.size()) {
      throw std::runtime_error("out of bounds");
    }
    --pos;
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
    while (!self::reserved::isEndl(*tmp)) {
      ++tmp;
      ++length;
    }
    return length;
  }
  size_t prevLineLength() const {
    TokenIt tmp = *this;
    size_t length = 0;
    while (!self::reserved::isEndl(*tmp)) {
      --tmp;
      ++length;
    }
    return length;
  }

private:
  TokenIt(self::LexedFileRef &where, size_t pos) : where(where), pos(pos) {}
};

struct ErrException {
  size_t col, line;
  std::string_view what;
};

bool coerceType(self::ExprPtr &e, self::TypePtr type) {
  if (auto *var = dynamic_cast<self::VarDeclaration *>(e.get())) {
    auto t = var->getRawName();
    if (!var->type_ref.ptr) {
      var->type_ref = type;
      if (type.depth == 0)
        throw std::runtime_error("var coercion depth should not be 0");
      --var->type_ref.depth;
      if (var->type_ref.depth == 0)
        var->type_ref.is_ref = self::RefTypes::value;
      return true;
    }
  } else if (auto *uneval =
                 dynamic_cast<self::UnevaluatedExpression *>(e.get())) {
    uneval->coerced_type = type;
    return true;
  }
  return e->getType() == type;
}

size_t tuple_count(self::ExprBase *e) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(e))
    return tuple->members.size();
  return 1;
}

void for_tuple(self::ExprPtr &e, auto unary) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(e.get())) {
    for (auto &a : tuple->members) {
      unary(*a);
    }
  } else
    unary(e);
}

void for_tuple(self::ExprPtr &left, auto gen, auto binary) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(left.get())) {
    for (auto member = tuple->members.begin(); member != tuple->members.end();
         ++member) {
      binary(*member, gen());
    }
  } else
    binary(left, gen());
}

void processSubtrees(self::ExprTree &tree, auto begin, auto end) {
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
            auto subtree = self::makeExpr<self::ExprTree>({open_paren->pos});
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

void processSubtrees(self::ExprTree &tree) {
  return processSubtrees(tree, tree.begin(), tree.end());
}

auto lookahead(auto it, auto &container) -> std::optional<decltype(it)> {
  if (std::end(container) == it)
    return std::nullopt;
  return ++it;
}
auto lookbehind(auto it, auto container) -> std::optional<decltype(it)> {
  if (std::begin(container) == it)
    return std::nullopt;
  return --it;
}

struct GlobalParser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  using TypeList = std::unordered_map<self::TokenView, self::TypeRef>;
  self::Context &c;
  self::ErrorList &err;
  std::vector<const self::StructDef *> &struct_list;

  void errReport(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }
  template <typename To> To expectCast(auto *from, std::string_view message) {
    static_assert(std::is_pointer_v<To>,
                  "type To must be either reference or pointer.");
    auto to = dynamic_cast<To>(from);
    errReport(to, message);
    return to;
  }
  template <typename To> To expectCast(auto &from, std::string_view message) {
    static_assert(std::is_reference_v<To>,
                  "type To must be either reference or pointer.");
    using T = std::remove_reference_t<To>;
    auto to = dynamic_cast<T *>(&from);
    errReport(to, message);
    return *to;
  }

  template <typename T> std::unique_ptr<T> upCast(self::ExprPtr &&unique) {
    auto *ptr = unique.get();
    auto *n = expectCast<T *>(ptr, "upcasting failed");
    ptr = unique.release();
    return std::unique_ptr<T>(n);
  }

  // notably there is only ever 1 tuple in the tree
  // since recursive tuples are processed first recursively
  void processTuples(self::ExprTree &tree, self::Index &context) {
    size_t size = 0;
    auto tuple = self::makeExpr<self::Tuple>(tree.at(0)->pos);
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
        tuple->members.emplace_back(evaluateTree(branch, context));
        size = 0;
      }
    }
    if (!tuple->members.empty()) {
      tuple->members.emplace_back(evaluateTree(tree, context));
      tuple->members.shrink_to_fit();
      tree.clear();
      tree.push_back(std::move(tuple));
    }
  }

  enum struct coerceResult { match, coerce, mismatch };

  coerceResult typeCoercible(self::TypePtr to, self::TypePtr from) {
    using enum coerceResult;
    if (to.ptr == from.ptr) {
      if (to.depth == from.depth)
        return match;
      if (to.depth - 1 == from.depth)
        return coerce;
    }
    if (from.ptr == &c.deduced_t && to.depth - 1 == from.depth) {
      return match;
    }
    return mismatch;
  }

  coerceResult needCoerce(self::ExprBase *e, self::TypePtr type) {
    using enum coerceResult;
    if (auto *var = dynamic_cast<self::VarDeclaration *>(e)) {
      if (!var->getDecltype().ptr) {
        return coerce;
      } else if (var->getDecltype() == type) {
        return match;
      }
    } else if (const auto *uneval =
                   dynamic_cast<const self::UnevaluatedExpression *>(e)) {
      return coerce;
    }

    return typeCoercible(e->getType(), type);
  }

  template <typename T, bool pre = true, bool post = true>
  auto processFunction(auto &it, bool not_a_member, auto lhsrhsinc, auto cond,
                       auto cleanup, auto insert, auto coerce,
                       self::Index &context) {
    if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      std::vector<const T *> no_coerce;
      std::vector<const T *> coerced;
      auto search = [&](auto candidates) {
        for (auto &[_, fun] : candidates) {
          const auto &op = dynamic_cast<const T &>(fun.get());
          if (not_a_member ^ op.member) {
            cond(&op, lhs, rhs, no_coerce, coerced);
          }
        }
      };
      search(self::pairRange(context.equalRange(T::qualify(t->getToken()))));
      // argument dependent lookup
      for (auto &hs : {lhs, rhs}) {
        if (auto *str = dynamic_cast<const self::StructDef *>(
                hs->get()->getType().ptr)) {
          search(self::pairRange(
              str->context->localEqualRange(T::qualify(t->getToken()))));
        }
      }
      if (!no_coerce.empty()) {
        errReport(no_coerce.size() == 1, "ambiguous operator call");
        coerce(*no_coerce.back(), *lhs, *rhs);
        auto result = insert(no_coerce.back(), lhs, rhs, no_coerce.back()->pos);
        cleanup(lhs, rhs);
        *it = std::move(result);
      } else if (!coerced.empty()) {
        errReport(coerced.size() == 1, "ambiguous operator call");
        coerce(*coerced.back(), *lhs, *rhs);
        auto result = insert(coerced.back(), lhs, rhs, coerced.back()->pos);
        cleanup(lhs, rhs);
        *it = std::move(result);
      }
    }
  }

  auto expectLookahead(auto it, auto &container, std::string_view error)
      -> decltype(it) {
    auto ahead = lookahead(it, container);
    errReport(ahead.has_value(), error);
    return *ahead;
  }

  void resolveMembers(self::ExprTree &container) {
    for (auto it = container.begin(); it != container.end(); ++it) {
      if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get());
          t && t->getToken() == ".") {
        auto behind = lookbehind(it, container).value();
        auto &behind_ptr = expectCast<self::VarDeref &>(
            **behind, "Dot operator expects a variable "
                      "dereference to the left");
        auto &struct_def = expectCast<const self::StructDef &>(
            *behind_ptr.definition.value->getType().ptr,
            "Dot operator should be used on structure type");
        auto ahead = expectLookahead(it, container, "Expected member name");
        auto &ahead_e = expectCast<self::UnevaluatedExpression &>(
            **ahead, "Unknown parsing error: Uneval expected");
        auto ahead_token = ahead_e.getToken();
        auto ahead_pos = ahead->get()->pos;
        auto n = self::VarDeclaration::qualify(ahead_token);
        if (auto v = struct_def.context->findLocally(n)) {
          auto structure = upCast<self::VarDeref>(std::move(*behind));
          auto &var = dynamic_cast<self::VarDeclaration &>(v->get());
          self::ExprPtr n = self::makeExpr<self::MemberDeref>(
              ahead_pos, var, std::move(structure), struct_def);
          behind->swap(n);
        } else if (auto f = struct_def.context->findLocally(
                       self::FunctionDef::qualify(ahead_token))) {
          auto &fun = dynamic_cast<self::FunctionDef &>(v->get());
          errReport(fun.arguments.front()->getRawName() == "this",
                    "Object-oriented-style function calls require 'this' "
                    "parameter as first parameter");
          auto arg_list = self::makeExpr<self::ArgPack>(ahead->get()->pos);
          arg_list->members.reserve(fun.argcount());
          arg_list->members.push_back(std::move(*behind));
          auto call = self::makeExpr<self::FunctionCall>(ahead_pos, fun);
          call->rhs = std::move(arg_list);
          *behind = std::move(call);
        }
        container.erase(ahead);
        container.erase(it);
        it = behind;
      }
    }
  }

  self::ExprPtr evaluateTree(self::ExprTree &tree, self::Index &local) {
    if (tree.empty()) {
      return self::makeExpr<self::Tuple>(tree.pos);
    }
    processSubtrees(tree);
    for (auto &ptr : tree) {
      if (auto *uneval = dynamic_cast<self::ExprTree *>(ptr.get())) {
        ptr = evaluateTree(*uneval, local);
      }
    }

    processTuples(tree, local);
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      if (auto *p = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
        if (p->getToken() == ".") {
          ++it;
          continue;
        }
      }
      *it = parseSymbol(it, tree, local);
    }
    resolveMembers(tree); // clang-format off
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wunused-parameter"
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      processFunction<self::FunctionDef>(
          it, true, [](auto &lhs, auto &rhs) { ++rhs; },
          //todo add type checking to fun param
          [](auto *fun, auto lhs, auto rhs,
             auto &perfect, auto& less_than) {
            if (fun->argcount() == tuple_count(rhs->get())) {
              perfect.push_back(fun);
              return true;
            }
            return false;
          },
          [&](auto lhs, auto rhs) { tree.erase(rhs); },
          [&](const self::FunctionDef *fun, auto lhs, auto rhs, self::Pos p) {
            auto result = self::makeExpr<self::FunctionCall>(p,*fun);
            if (auto *tuple = dynamic_cast<self::Tuple *>(rhs->get())) {
              result->rhs =
                  std::make_unique<self::ArgPack>(std::move(*tuple));
            } else {
              result->rhs = std::move(*rhs);
            }
            return result;
          }, [](const self::FunctionDef& fun, auto& lhs, auto& rhs){
            int i = 0;
            for_tuple(rhs, [&]{
              return fun.arguments.at(i++)->getDecltype();},
              [](self::ExprPtr& e, auto type){coerceType(e, type);});
          }, local);
    }
    #pragma clang diagnostic pop // clang-format on

    auto binCondition = [&](auto *op, auto lhs, auto rhs, auto &no_coerce,
                            auto &coerce_r) {
      using enum coerceResult;
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
    auto binInsert = [](const self::OperatorDef *o, auto lhs, auto rhs,
                        self::Pos p) {
      auto result = self::makeExpr<self::FunctionCall>(p, *o);
      if (auto *var = dynamic_cast<self::VarDeclaration *>(lhs->get())) {
        var->value = rhs->get();
      }
      result->lhs = std::move(*lhs);
      result->rhs = std::move(*rhs);
      return result;
    };
    auto bin_coerce = [this](const self::OperatorDef &fun, auto &lhs,
                             auto &rhs) {
      if (fun.internal == self::detail::assign ||
          fun.internal == self::detail::store) {
        auto &var = dynamic_cast<self::VarDeclaration &>(*lhs);
        var.value = rhs.get();
      }
      if (auto n = fun.lhs->getDecltype(); n.ptr == &c.deduced_t) {
        coerceType(lhs, rhs.get()->getType());
      } else {
        coerceType(lhs, n);
      }
      coerceType(rhs, fun.rhs->getDecltype());
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
    return std::move(tree.back());
  }

  // maybe reuse this?
  void tryCoerceFun(auto &args, auto &ctor, auto end, auto arg_it,
                    auto &perfect, auto &coerced) {
    for (auto arg : args) {
      if (arg_it == end)
        return;
      auto type = arg_it->get()->getDecltype();
      if (auto r = needCoerce(arg, type); r == coerceResult::mismatch)
        return;
      else {
        if (r == coerceResult::match) {
          perfect.push_back(&ctor);
          return;
        } else {
          coerced.push_back(&ctor);
          return;
        }
      }
    }
  }

  self::FunctionDef *findCtor(self::StructDef &structure,
                              std::span<self::ExprBase *> args) {
    auto candidates = structure.context->localEqualRange(
        self::FunctionDef::qualify("struct"));
    std::vector<self::FunctionDef *> perfect;
    std::vector<self::FunctionDef *> coerced;
    for (auto &[_, f] : self::pairRange(candidates)) {
      auto &ctor = dynamic_cast<self::FunctionDef &>(f.get());
      if (ctor.argcount() != args.size())
        continue;
      tryCoerceFun(args, ctor, ctor.arguments.end(), ctor.arguments.begin(),
                   perfect, coerced);
    }
    if (!perfect.empty()) {
      errReport(perfect.size() == 1, "ambiguous constructor call");
      return perfect.back();
    } else {
      errReport(!coerced.empty(), "No valid constructor calls");
      errReport(coerced.size() == 1, "ambiguous constructor call");
      return coerced.back();
    }
  }

  self::ExprPtr ctorCheck(auto it, auto &container, auto &var, auto pos) {
    auto next = lookahead(it, container).value();
    // checks for constructor call
    auto *structure = dynamic_cast<self::StructLit *>(var.value);
    if (var.getDecltype().ptr != &c.type_t || !*next || !structure) {
      return self::makeExpr<self::VarDeref>(pos, var);
    }
    self::FunctionDef *ctor = nullptr;
    if (auto next_type = next->get()->getType(); next_type.ptr) {
      auto args = std::array{next->get()};
      ctor = findCtor(structure->value, args);
      coerceType(*next, ctor->arguments.back()->getDecltype());
    } else if (auto *tuple = dynamic_cast<self::Tuple *>(next->get())) {
      std::vector<self::ExprBase *> exprs;
      exprs.reserve(tuple->members.size());
      std::for_each(tuple->members.begin(), tuple->members.end(),
                    [&](self::ExprPtr &e) { exprs.push_back(e.get()); });
      ctor = findCtor(structure->value, exprs);
      auto arg_pack =
          self::makeExpr<self::ArgPack>(tuple->pos, std::move(*tuple));
      auto decl = ctor->arguments.begin();
      auto arg = arg_pack->members.begin();
      while (decl != ctor->arguments.end() && arg != arg_pack->members.end()) {
        coerceType(*arg, decl->get()->getDecltype());
        ++decl, ++arg;
      }
      *next = std::move(arg_pack);
    } else {
      return self::makeExpr<self::VarDeref>(pos, var);
    }
    auto result = self::makeExpr<self::FunctionCall>(pos, *ctor);
    result->rhs = std::move(*next);
    container.erase(next);
    return result;
  }

  self::ExprPtr parseSymbol(auto it, auto &container, self::Index &local) {
    auto pos = it->get()->pos;
    if (auto *maybe = dynamic_cast<self::UnevaluatedExpression *>(it->get());
        maybe && !maybe->isComplete()) {
      auto t = maybe->getToken();
      if (auto [is_int, number] = isInt(t); is_int) {
        return self::makeExpr<self::IntLit>(pos, number, c);
      } else if (auto str = isStr(t)) {
        return self::makeExpr<self::StringLit>(pos, *str, c);
      } else if (auto boolean = isBool(t)) {
        return self::makeExpr<self::BoolLit>(pos, *boolean, c);
      } else if (self::BuiltinTypeLit::contains(t, c)) {
        return self::makeExpr<self::BuiltinTypeLit>(
            pos, self::BuiltinTypeLit::get(t, c));
      } else if (auto varname = self::VarDeclaration::qualify(t);
                 local.contains(varname)) {
        errReport(local.isUnique(varname), "ODR var declaration rule violated");
        auto &var =
            dynamic_cast<self::VarDeclaration &>(local.find(varname)->get());
        return ctorCheck(it, container, var, pos);
      }
    }
    return std::move(*it);
  }
  using callback = std::function<void(self::ExprTree &)>;

  constexpr static auto default_end = [](self::TokenView t) -> bool {
    return self::reserved::isEndl(t) || t == "}";
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
        tree.push_back(
            self::makeExpr<self::UnevaluatedExpression>({t.col, t.line}, *t++));
      }
    }
    return self::foldExpr(evaluateTree(tree, context), context).first;
  }

  self::ExprPtr parseVar(TokenIt &t, self::TokenView name,
                         self::Index &context) {
    using namespace self::reserved;
    if (!notReserved(name)) {
      std::stringstream err;
      err << "Token " << name << " is reserved";
      errReport(false, err.str());
    }
    auto curr = self::makeExpr<self::VarDeclaration>(t.coord(), name);
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
    if (self::reserved::isEndl(*t))
      ++t;
  }

  std::unique_ptr<self::Block> forceBlock(TokenIt &t, self::Block &parent) {
    // this exists to silence a warning about side-effects in typeid expressions
    if (*t == "{") {
      ++t;
      return self::makeExpr<self::Block>(t.coord(),
                                         parseBlock(t, parent.contexts));
    }
    auto results = self::makeExpr<self::Block>(t.coord(), parent.contexts);
    results->push_back(parseExpr(t, results->contexts));
    return results;
  };

  void parseIf(TokenIt &t, self::Block &body) {
    ++t;
    auto if_statement = self::makeExpr<self::If>(t.coord());

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
    auto p = t.coord();
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
    body.push_back(self::makeExpr<self::While>(p, std::move(block),
                                               std::move(condition), is_do));
  }

  self::Block
  parseBlock(TokenIt &t, self::Index &parent,
             std::function<void(self::Block &)> callback = nullptr) {
    auto body = self::Block(parent);
    if (callback)
      callback(body);
    while (*t != "}") {
      using namespace self::reserved;
      if (*t == var_t) {
        auto name = *++t;
        body.push_back(parseVar(++t, name, body.contexts));
      } else if (*t == return_t) {
        ++t;
        if (!self::reserved::isEndl(*t)) {
          body.push_back(self::makeExpr<self::Ret>(
              t.coord(), parseExpr(t, body.contexts)));
        } else {
          body.push_back(self::makeExpr<self::Ret>(t.coord()));
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

  std::unique_ptr<self::FunctionDef> parseFun(TokenIt &t, self::TokenView name,
                                              self::Index &parent) {
    auto curr = self::makeExpr<self::FunctionDef>(t.coord(), name, parent);
    errReport(*t++ == "(", "\"(\" expected");
    while (*t != ")") {
      errReport(notReserved(*t),
                "reserved Token cannot be used as parameter name");
      curr->arguments.emplace_back(
          self::makeExpr<self::VarDeclaration>(t.coord(), *t++));
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
        return self::reserved::isEndl(t) || t == "{";
      };
      auto e = parseExpr(t, parent, nullptr, commaOrBracket);
      auto type = self::getLiteralType(*e);
      curr->return_type = {&type.ptr, type.is_ref};
      curr->body_defined = false;
    }
    if (*t == "{") {
      ++t;
      curr->body.emplace(parseBlock(t, parent, [&](self::Block &b) {
        for (auto &arg : curr->arguments) {
          b.contexts.insert({arg->getName(), *arg});
        }
      }));
      curr->body_defined = true;
      if (curr->return_type.ptr == nullptr) {
        self::TypePtr type{};
        for (auto &expr : *curr->body) {
          if (auto ret = dynamic_cast<self::Ret *>(expr.get())) {
            if (!type.ptr) {
              type = ret->getRetType(c);
            } else {
              errReport(type != ret->getRetType(c),
                        "Cannot deduce return type of function.");
            }
          }
        }
        if (type.ptr == nullptr) {
          curr->return_type.ptr = &c.void_t;
          if (!dynamic_cast<self::Ret *>(curr->body->back().get())) {
            curr->body->push_back(self::makeExpr<self::Ret>(t.coord()));
          }
        } else {
          curr->return_type = type;
        }
      }
    }
    parent.insert({curr->name, *curr});
    return curr;
  }

  std::unique_ptr<self::StructLit> parseStruct(TokenIt &t,
                                               self::Index &parent) {
    auto str = makeStruct(t, parent);
    struct_list.push_back(&str->value);
    return str;
  }

  std::unique_ptr<self::StructLit> makeStruct(TokenIt &t, self::Index &parent) {
    static unsigned int id = 0;
    auto identity = std::string("struct");
    identity.append(std::to_string(id++));
    errReport(*t == "{" || *t == "(", "Expected a \"{\" or \"(\"");
    auto p = t.coord();
    if (*t == "(") {
      ++t;
      if (*t == ")") {
        ++t;
        return self::makeExpr<self::StructLit>(p, self::StructDef(0, parent),
                                               c);
      } else {
        auto [success, size] = isInt(*t++);
        errReport(success, "Expected integer or \")\"");
        return self::makeExpr<self::StructLit>(p, self::StructDef(size, parent),
                                               c);
      }
    } else {
      auto result = self::StructDef(parent);
      ++t;
      while (*t != "}") {
        if (*t == "var") {
          auto name = *++t;
          result.body.push_back(parseVar(++t, name, *result.context));
        } else if (*t == "fun") {
          auto name = *++t;
          errReport(notReserved(name), "function name is reserved");
          result.body.push_back(parseFun(++t, name, *result.context));
        } else {
          errReport(self::reserved::isEndl(*t),
                    "expected a function or variable declaration.");
          ++t;
        }
      }
      ++t;
      result.identity = id;
      auto ret = self::makeExpr<self::StructLit>(p, std::move(result), c);
      configStruct(ret);
      return ret;
    }
  }

  void configStruct(std::unique_ptr<self::StructLit> &str) {
    std::vector<std::unique_ptr<self::VarDeclaration>> args;
    for (auto &member : str->value.body) {
      if (auto *var_decl = dynamic_cast<self::VarDeclaration *>(member.get())) {
        args.push_back(std::make_unique<self::VarDeclaration>(*var_decl));
      }
    }
    args.shrink_to_fit();
    auto ctor = std::make_unique<self::FunctionDef>(
        "struct", *str->value.context, str->value, true, std::move(args));
    ctor->return_type = {str->value};
    str->value.insert(std::move(ctor));
    str->value.insert(std::make_unique<self::OperatorDef>(
        "=", self::TypeRef{str->value, self::RefTypes::ref},
        self::VarDeclarationPtr("this",
                                self::TypeRef(str->value, self::RefTypes::ref)),
        self::VarDeclarationPtr("value", str->value), *str->value.context,
        self::detail::store, true));
    str->value.body.shrink_to_fit();
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
    auto a = isStr(*t);
    errReport(!a->empty(), err);
    std::string unconvert;
    unconvert.reserve(a->size());
    std::for_each(a->begin(), a->end(),
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

  // todo process qualifiers
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
          auto f = parseFun(++t, name, global);
          if (f->body_defined) {
            f->qualifiers = self::Qualifiers::qExport;
          }
          syntax_tree.push_back(std::move(f));
        } else if (notReserved(*t)) {
          syntax_tree.push_back(parseExpr(t, global));
        } else if (*t == import_t) {
          processImport(++t, syntax_tree, global);
        } else if (*t == "extern") {
          processExtern(++t, syntax_tree);
        } else {
          errReport(self::reserved::isEndl(*t++), "invalid expression");
        }
      }
    } catch (std::runtime_error e) {
      err.errors.push_back(self::Error{t.col, t.line, e.what()});
      while (*t != ";")
        ++t;
      goto retry;
    }
  }

  self::ExprTree process(TokenIt t, self::Index &i) {
    self::ExprTree syntax_tree;
    process(t, syntax_tree, i);
    syntax_tree.shrink_to_fit();
    return syntax_tree;
  }

  GlobalParser(self::Context &c, self::ErrorList &e,
               std::vector<const self::StructDef *> &list)
      : c(c), err(e), struct_list(list) {}
};
} // namespace

namespace self {
Module parse(LexedFileRef &in, Context &c) {
  ErrorList e;
  std::vector<const self::StructDef *> list;
  auto parser = GlobalParser(c, e, list);
  auto root = Index(c.root);
  auto ast = parser.process(TokenIt{in}, root);
  return Module(std::move(root), std::move(ast), std::move(e), std::move(list));
}
} // namespace self