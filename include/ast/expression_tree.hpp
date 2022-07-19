#pragma once

#include <algorithm>
#include <memory>
#include <sstream>

#include "ast/expression.hpp"

namespace self {
struct ExprTree : public ExprImpl<ExprTree>, public ExpressionList {
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
  ExprTree() = default;
  ExprTree(const ExprTree &other) {
    std::for_each(other.cbegin(), other.cend(),
                  [&](const ExprPtr &p) { this->push_back(p->clone()); });
  }
};

struct Namespace : ExprTree {
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