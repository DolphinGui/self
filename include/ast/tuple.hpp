#pragma once

#include "expression.hpp"
#include "expression_tree.hpp"
#include <memory>
#include <vector>

namespace selflang {
struct tuple : public expression {
  tuple() = default;
  tuple(expression_tree &&contents) : members(std::move(contents)) {}
  expression_tree members;
  std::ostream &print(std::ostream &out) const override {
    out << '(';
    for (auto &m : members) {
      out << *m << ", ";
    }
    return out << ')';
  }
  token_view getName() const noexcept override { return "tuple"; };
};
} // namespace selflang