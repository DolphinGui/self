#pragma once

#include "ast/unevaluated_expression.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include <charconv>
#include <cstdint>
#include <stdexcept>

namespace self {
namespace detail {
namespace parser {
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
  Pos pos;
  std::string what;
};

// assumes ASCII numerals, might want to support other numeral types later
inline std::optional<size_t> isInt(self::TokenView t) {
  size_t number = 0;
  auto [ptr, ec] = std::from_chars(t.data(), t.data() + t.length(), number);
  if (ec != std::errc())
    return number;
  return std::nullopt;
}

auto notReserved(auto t) {
  return !self::reserved::isKeyword(t) && !self::reserved::isGrammar(t);
};

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
inline void errReport(bool condition, Pos p, std::string message) {
  if (!condition) {
    throw ErrException{.pos = p, .what = std::move(message)};
  }
}

inline void errReport(bool condition, TokenIt &t, std::string message) {
  errReport(condition, t.coord(), std::move(message));
  ++t;
}

inline void expectToken(TokenView tok, TokenIt &t, std::string message) {
  errReport(*t == tok, t.coord(), std::move(message));
  ++t;
}

template <typename To> To expectCast(auto *from, std::string_view message) {
  static_assert(std::is_pointer_v<To>,
                "type To must be either reference or pointer.");
  auto to = dynamic_cast<To>(from);
  if (!to)
    throw std::runtime_error(message.data());
  return to;
}

template <typename To> To expectCast(auto &from, std::string_view message) {
  static_assert(std::is_reference_v<To>,
                "type To must be either reference or pointer.");
  using T = std::remove_reference_t<To>;
  auto to = dynamic_cast<T *>(&from);
  if (!to)
    throw std::runtime_error(message.data());
  return *to;
}

template <typename T> std::unique_ptr<T> upCast(self::ExprPtr &&unique) {
  auto *ptr = unique.get();
  auto *n = expectCast<T *>(ptr, "upcasting failed");
  ptr = unique.release();
  return std::unique_ptr<T>(n);
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

auto expectLookahead(auto it, auto &container, std::string_view error)
    -> decltype(it) {
  auto ahead = lookahead(it, container);
  if (ahead.has_value())
    throw std::runtime_error(error.data());
  return *ahead;
}

inline void consumeNullExpr(TokenIt &t) {
  if (self::reserved::isEndl(*t))
    ++t;
}

inline bool default_end(self::TokenView t) {
  return self::reserved::isEndl(t) || t == "}";
};

enum struct coerceResult { match, coerce, mismatch };
coerceResult typeCoercible(self::Context &c, self::TypePtr to,
                           self::TypePtr from);

coerceResult needCoerce(self::Context &c, self::ExprBase *e,
                        self::TypePtr type);

bool coerceType(self::ExprPtr &e, self::TypePtr type);

struct GlobalParser {
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  using TypeList = std::unordered_map<self::TokenView, self::TypeRef>;
  self::Context &c;
  self::ErrorList &err;
  std::vector<const self::StructDef *> &struct_list;

  using callback = std::function<void(self::ExprTree &)>;

  self::ExprPtr
  parseExpr(TokenIt &t, self::Index &context, callback start = nullptr,
            std::function<bool(self::TokenView)> endExpr = &default_end);

  self::ExprPtr parseVar(TokenIt &t, self::TokenView name,
                         self::Index &context);

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
  self::ExprTree process(TokenIt t, self::Index &i);

  GlobalParser(self::Context &c, self::ErrorList &e,
               std::vector<const self::StructDef *> &list)
      : c(c), err(e), struct_list(list) {}
};

} // namespace parser
} // namespace detail
} // namespace self