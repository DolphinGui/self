#pragma once

#include "expression.hpp"
#include "expression_tree.hpp"
#include <memory>
#include <vector>

namespace self {
struct Tuple : public Expression {
  Tuple() = default;
  Tuple(ExpressionTree &&contents) : members(std::move(contents)) {}
  ExpressionTree members;
  std::ostream &print(std::ostream &out) const override {
    out << '(';
    for (auto &m : members) {
      out << *m << ", ";
    }
    return out << ')';
  }
  TokenView getName() const noexcept override { return "tuple"; };
};
struct arg_pack : public Tuple {
  arg_pack() = default;
  arg_pack(Tuple &&other) : Tuple(std::move(other)) {}
};
} // namespace self