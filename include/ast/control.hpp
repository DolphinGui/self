#pragma once
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "builtins.hpp"
#include <memory>
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
  TypePtr getRetType(self::Context &c) const noexcept {
    if (!value)
      return {c.void_t};
    else
      return value->getType();
  }
};

struct If : public ExprImpl<If> {
  std::unique_ptr<Block> block;
  ExprPtr condition;
  // This may be another If for a elif
  // or just another ExpressionTree
  std::unique_ptr<Block> else_block;
  If(std::unique_ptr<Block> &&code, ExprPtr &&cond)
      : block(std::move(code)), condition(std::move(cond)) {}
  If() = default;
  If(const If &other)
      : block(std::make_unique<Block>(*other.block)),
        condition(other.condition->clone()) {}
  virtual TokenView getName() const noexcept override { return "if"; }
  inline std::ostream &print(std::ostream &out) const override {
    out << "if " << *condition;
    out << ":\n" << *block;
    if (else_block) {
      out << "\nelse: " << *else_block;
    }
    return out;
  }
  bool isComplete() const noexcept override { return condition && block; }
};
struct While : public ExprImpl<While> {
  std::unique_ptr<Block> block;
  ExprPtr condition;

  // true when it's a do{}while
  bool is_do = false;
  While(std::unique_ptr<Block> &&code, ExprPtr &&cond, bool is_do)
      : block(std::move(code)), condition(std::move(cond)), is_do(is_do) {}
  While(const While &other)
      : block(std::make_unique<Block>(*other.block)),
        condition(other.condition->clone()), is_do(other.is_do) {}
  virtual TokenView getName() const noexcept override { return "while"; }
  inline std::ostream &print(std::ostream &out) const override {
    if (is_do) {
      out << "do:\n";
    } else {
      out << "while " << *condition << ":\n";
    }
    out << *block;
    if (is_do) {
      out << "while " << *condition;
    }
    return out;
  }
  bool isComplete() const noexcept override { return condition && block; }
};
} // namespace self