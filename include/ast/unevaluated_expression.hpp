#pragma once

#include "ast/expression.hpp"

namespace self {
class UnevaluatedExpression : public Expression {
  Token contents;

public:
  TypePtr coerced_type = {nullptr};

  UnevaluatedExpression(TokenView t) : contents(t) {}
  inline TokenView getToken() { return contents; }
  virtual std::ostream &print(std::ostream &stream) const override {
    return stream << "unevaluated expression: \"" << contents << "\" ";
  }
  virtual bool isComplete() const override { return false; }
  virtual TokenView getName() const noexcept override {
    return "unevaluated expression";
  }

  ExpressionPtr clone() const override {
    return std::make_unique<UnevaluatedExpression>(*this);
  }
};
} // namespace self