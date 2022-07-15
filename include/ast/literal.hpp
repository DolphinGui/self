#pragma once

#include <concepts>
#include <functional>
#include <ostream>
#include <typeinfo>
#include <unordered_map>

#include "ast/expression.hpp"
#include "ast/variables.hpp"

namespace self {
template <typename T> struct Literal : public Expression {
  T value;
  TypeRef type;
  inline std::ostream &print(std::ostream &out) const override {
    out << "literal: ";

    if constexpr (std::derived_from<T, self::Type>) {
      out << value.getTypename();
    } else {
      out << value;
    }
    return out;
  }
  inline TokenView getName() const noexcept override {
    return "literal value";
  };
  Literal(T &&value, TypeRef type) : value(std::move(value)), type(type) {}
  Literal(const T &value, TypeRef type) : value(value), type(type) {}
  TypePtr getType() const noexcept override {
    return TypePtr(&type.ptr, RefTypes::value);
  }
};

} // namespace self