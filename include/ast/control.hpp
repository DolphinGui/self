#pragma once
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
namespace self {
struct Ret : public ExprImpl<Ret> {
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
};

struct If : public ExprImpl<If> {
  ExpressionTree block;
  ExpressionPtr condition;
  // This may be another If for a elif
  // or just another ExpressionTree
  ExpressionPtr else_code;
  If(ExpressionTree &&code, ExpressionPtr &&cond)
      : block(std::move(code)), condition(std::move(cond)) {}
  If() = default;
  If(const If &other)
      : block(other.block), condition(other.condition->clone()) {}
  virtual TokenView getName() const noexcept override { return "return"; }
  inline std::ostream &print(std::ostream &out) const override {
    out << "if: " << block;
    if (else_code) {
      out << "\nelse: " << *else_code;
    }
    return out;
  }
  bool isComplete() const noexcept override {
    return condition && !block.empty();
  }

  ExpressionPtr clone() const override { return std::make_unique<If>(*this); }
};
} // namespace self