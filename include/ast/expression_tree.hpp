#pragma once

#include <sstream>

#include "ast/expression.hpp"

namespace self {
struct ExpressionTree : public Expression, public ExpressionList {
  inline std::ostream &print(std::ostream &out) const override {
    out << "Tree contents:\n";
    for (const auto &e : *this) {
      out << "  " << *e << '\n';
    }
    return out;
  }

  // for debugging
  std::string dump() {
    std::stringstream result;
    print(result);
    return result.str();
  }

  virtual TokenView getName() const noexcept override {
    return "expression tree";
  }
  bool isComplete() const noexcept override {
    for (auto &expr : *this) {
      if (!expr->isComplete())
        return false;
    }
    return true;
  }
  void complete_types();
};
struct namespace_tree : ExpressionTree {
  Token name;
  inline std::ostream &print(std::ostream &out) const override {
    out << "namespace " << name << " contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
  }
};
} // namespace self