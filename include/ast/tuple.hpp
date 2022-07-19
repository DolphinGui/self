#pragma once

#include "expression.hpp"
#include "expression_tree.hpp"
#include <algorithm>
#include <memory>
#include <vector>

namespace self {
struct Tuple : public ExprImpl<Tuple> {
  Tuple() = default;
  Tuple(ExpressionTree &&contents) : members(std::move(contents)) {}
  Tuple(const Tuple &other) {
    std::for_each(other.members.cbegin(), other.members.cend(),
                  [&](const auto &e) { members.push_back(e->clone()); });
  }
  ExpressionTree members;
  std::ostream &print(std::ostream &out) const override {
    out << '(';
    for (auto &m : members) {
      out << *m << ", ";
    }
    return out << ')';
  }
  TokenView getName() const noexcept override { return "tuple"; }
};
struct arg_pack : public Tuple {
  arg_pack() = default;
  arg_pack(Tuple &&other) : Tuple(std::move(other)) {}
};
} // namespace self