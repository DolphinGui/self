#pragma once

#include "ast/visitor.hpp"
#include "expression.hpp"
#include "expression_tree.hpp"
#include <algorithm>
#include <memory>
#include <vector>

namespace self {
struct Tuple : public ExprImpl<Tuple> {
  Tuple() = default;
  Tuple(ExprTree &&contents) : members(std::move(contents)) {}
  Tuple(const Tuple &other) {
    std::for_each(other.members.cbegin(), other.members.cend(),
                  [&](const auto &e) { members.push_back(e->clone()); });
  }
  ExprTree members;
  std::ostream &print(std::ostream &out) const override {
    out << '(';
    for (auto &m : members) {
      out << *m << ", ";
    }
    return out << ')';
  }
  TokenView getName() const noexcept override { return "tuple"; }
};
struct ArgPack : public Tuple {
  ArgPack() = default;
  ArgPack(Tuple &&other) : Tuple(std::move(other)) {}
};
} // namespace self