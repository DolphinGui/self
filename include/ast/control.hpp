#pragma once
#include "ast/expression.hpp"
namespace selflang {
struct ret : public expression {
  expression_ptr value;
  ret(expression_ptr &&val) : value(std::move(val)) {}
  ret() = default;
  virtual token_view getName() const noexcept override { return "return"; }
  inline std::ostream &print(std::ostream &out) const override {
    if (value)
      out << "returns " << *value;
    return out;
  }
  bool isComplete() const noexcept override { 
    if(value){
      return value->isComplete();
    }
    return true;
   }
};
} // namespace selflang