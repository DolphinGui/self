#pragma once
#include "ast/expression.hpp"
namespace self {
struct Ret : public Expression {
  ExpressionPtr value;
  Ret(ExpressionPtr &&val) : value(std::move(val)) {}
  Ret() = default;
  Ret(const Ret &other) : value(other.clone()) {}
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

  ExpressionPtr clone() const override { return std::make_unique<Ret>(*this); }
};
} // namespace self