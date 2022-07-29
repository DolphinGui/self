#pragma once

#include "ast/visitor.hpp"
#include "ast/expression.hpp"

namespace self {
class UnevaluatedExpression : public ExprImpl<UnevaluatedExpression> {
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
};
} // namespace self