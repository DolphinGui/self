#pragma once
#include "ast/expression.hpp"
namespace self {
struct Ret : public Expression {
  ExpressionPtr value;
  Ret(ExpressionPtr &&val) : value(std::move(val)) {}
  Ret() = default;
  virtual TokenView getName() const noexcept override { return "return"; }
  inline std::ostream &print(std::ostream &out) const override {
    if (value)
      out << "returns " << *value;
    return out;
  }
  bool isComplete() const noexcept override {
    if (value) {
      return value->isComplete();
    }
    return true;
  }
};
} // namespace self