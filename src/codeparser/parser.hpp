#pragma once

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/tuple.hpp"
#include "ast/unevaluated_expression.hpp"
#include "builtins.hpp"
#include "error_handling.hpp"
#include "ffi_parse.hpp"
#include "lexer.hpp"
#include "literals.hpp"

#include <fstream>
#include <span>
#include <string>

namespace self {
namespace detail {
namespace parser {

inline auto isInt(self::TokenView t) {
  char *p;
  auto number = std::strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}

inline void escape(std::vector<unsigned char> &literal) {
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

inline std::vector<unsigned char> convertString(std::string_view s) {
  std::vector<unsigned char> result;
  result.reserve(s.size());
  std::for_each(s.cbegin(), s.cend(), [&](char c) { result.push_back(c); });
  return result;
}

inline std::optional<std::vector<unsigned char>> isStr(self::TokenView t) {
  if (t.front() != '\'' && t.front() != '\"')
    return std::nullopt;
  if (!t.ends_with('\'') && !t.ends_with('\"'))
    return std::nullopt;
  auto literal = convertString(t);
  escape(literal);
  return literal;
}

inline std::optional<bool> isBool(self::TokenView t) {
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

struct ErrException {
  size_t col, line;
  std::string what;
};

inline void errReport(bool condition, Pos p, std::string message) {
  if (!condition) {
    throw ErrException{p.col, p.line, std::move(message)};
  }
}

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

struct GlobalParser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  using TypeList = std::unordered_map<self::TokenView, self::TypeRef>;
  self::Context &c;
  self::ErrorList &err;
  std::vector<const self::StructDef *> &struct_list;

  self::ExprPtr evaluateTree(self::ExprTree &tree, self::Index &local);

  using callback = std::function<void(self::ExprTree &)>;

  constexpr static auto default_end = [](self::TokenView t) -> bool {
    return self::reserved::isEndl(t) || t == "}";
  };

  self::ExprPtr
  parseExpr(TokenIt &t, self::Index &context, callback start = nullptr,
            std::function<bool(self::TokenView)> endExpr = default_end);

  self::ExprPtr parseVar(TokenIt &t, self::TokenView name,
                         self::Index &context);

  void consumeNullExpr(TokenIt &t) {
    if (self::reserved::isEndl(*t))
      ++t;
  }

  std::unique_ptr<self::Block> forceBlock(TokenIt &t, self::Block &parent);

  void parseIf(TokenIt &t, self::Block &body);

  void parseWhile(TokenIt &t, self::Block &body);

  self::Block parseBlock(TokenIt &t, self::Index &parent,
                         std::function<void(self::Block &)> callback = nullptr);

  std::unique_ptr<self::FunctionDef> parseFun(TokenIt &t, self::TokenView name,
                                              self::Index &parent);

  std::unique_ptr<self::StructLit> parseStruct(TokenIt &t, self::Index &parent);

  void processImport(TokenIt &t, self::ExprTree &syntax_tree,
                     self::Index &global);
  void processExtern(TokenIt &t, self::ExprTree &syntax_tree);

  // todo process qualifiers
  void process(TokenIt t, self::ExprTree &syntax_tree, self::Index &global);

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
} // namespace parser
} // namespace detail
} // namespace self