#pragma once
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
namespace self {
struct Ret : public ExprImpl<Ret> {
  ExprPtr value;
  Ret(ExprPtr &&val) : value(std::move(val)) {}
  Ret() = default;
  Ret(const Ret &other) : value(other.value->clone()) {}
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
  ExprPtr block;
  ExprPtr condition;
  // This may be another If for a elif
  // or just another ExpressionTree
  ExprPtr else_block;
  If(ExprPtr &&code, ExprPtr &&cond)
      : block(std::move(code)), condition(std::move(cond)) {}
  If() = default;
  If(const If &other)
      : block(other.block->clone()), condition(other.condition->clone()) {}
  virtual TokenView getName() const noexcept override { return "return"; }
  inline std::ostream &print(std::ostream &out) const override {
    out << "if: " << block;
    if (else_block) {
      out << "\nelse: " << *else_block;
    }
    return out;
  }
  bool isComplete() const noexcept override { return condition && !block; }
};
} // namespace self