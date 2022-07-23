#pragma once

#include <algorithm>
#include <memory>
#include <sstream>
#include <vector>

#include "ast/Index.hpp"
#include "ast/expression.hpp"

namespace self {
struct ExprTree : public ExprImpl<ExprTree>, public ExpressionList {
  virtual inline std::ostream &print(std::ostream &out) const override {
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
  ExprTree(ExprTree &&other) : ExpressionList(std::move(other)) {}
  ExprTree(const ExprTree &other) {
    std::for_each(other.cbegin(), other.cend(),
                  [&](const ExprPtr &p) { this->push_back(p->clone()); });
  }
};

struct Namespace : public ExprTree {
  Token name;

  inline std::ostream &print(std::ostream &out) const override {
    out << "namespace " << name << " contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
  }
};
struct Block : public ExprTree {
  // this is almost certainly not optimal, but I can't think of anything else
  Index contexts;
  Block(Index &i) : contexts(i) {}
  Block(Block &&other)
      : ExprTree(std::move(other)), contexts(std::move(other.contexts)) {}
  Block(const Block &other) : ExprTree(other), contexts(other.contexts) {}
  inline std::ostream &print(std::ostream &out) const override {
    out << "{ ";
    for (const auto &e : *this) {
      out << "  " << *e << '\n';
    }
    out << '}';
    return out;
  }
};
} // namespace self