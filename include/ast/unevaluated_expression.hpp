#pragma once

#include "ast/expression.hpp"

namespace selflang {
class unevaluated_expression : public expression {
  token contents;

public:
  type_ptr coerced_type = {nullptr};

  unevaluated_expression(token_view t) : contents(t) {}
  inline token_view get_token() { return contents; }
  virtual std::ostream &print(std::ostream &stream) const override {
    return stream << "unevaluated expression: \"" << contents << "\" ";
  }
  virtual bool isComplete() const override { return false; }
  virtual token_view getName() const noexcept override {
    return "unevaluated expression";
  }
};
} // namespace selflang