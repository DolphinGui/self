#pragma once

#include <concepts>
#include <functional>
#include <ostream>
#include <typeinfo>
#include <unordered_map>
#include <utility>

#include "ast/expression.hpp"
#include "ast/variables.hpp"

namespace self {

namespace detail {
template <class, class = void> struct hasPrint : std::false_type {};
template <class T>
struct hasPrint<T, std::void_t<decltype(std::declval<T>().printval(
                       std::declval<std::ostream &>()))>> : std::true_type {};
}; // namespace detail
struct Literal {};
template <typename T, typename Derive>
struct LiteralImpl : public Expression, public Literal {
  T value;
  TypeRef type;
  inline std::ostream &print(std::ostream &out) const override {
    out << "literal: ";
    if constexpr (std::derived_from<T, self::Type>) {
      out << value.getTypename();
    } else if constexpr (detail::hasPrint<Derive>::value) {
      static_cast<const Derive &>(*this).printval(out);
    } else {
      out << value;
    }
    return out;
  }
  inline TokenView getName() const noexcept override {
    return "literal value";
  };
  LiteralImpl(T &&value, TypeRef type) : value(std::move(value)), type(type) {}
  LiteralImpl(const T &value, TypeRef type) : value(value), type(type) {}
  TypePtr getType() const noexcept override {
    return TypePtr(&type.ptr, RefTypes::value);
  }
  bool isCompiletime() const noexcept override { return true; }
  bool operator==(const LiteralImpl &other) const noexcept {
    return other.value == value;
  }

  ExpressionPtr clone() const override {
    return std::make_unique<LiteralImpl>(*this);
  }
};

} // namespace self