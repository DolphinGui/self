#pragma once

#include "ast/expression.hpp"

namespace selflang {
template <typename T> struct literal : public expression {
  T value;
  literal(T itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "literal: " << value;
  }
  inline token_view getName() const noexcept override {
    return "literal value";
  };
};
} // namespace selflang