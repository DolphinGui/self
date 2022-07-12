#pragma once

#include <sstream>

#include "ast/expression.hpp"

namespace selflang {
struct expression_tree : public expression, public expression_list {
  inline std::ostream &print(std::ostream &out) const override {
    out << "Tree contents:\n";
    for (const auto &e : *this) {
      out << "  " << *e << '\n';
    }
    return out;
  }
  std::string dump_contents() {
    std::stringstream result;
    print(result);
    return result.str();
  }
  virtual token_view getName() const noexcept override {
    return "expression tree";
  }
  bool is_complete() const noexcept override {
    for (auto &expr : *this) {
      if (!expr->is_complete())
        return false;
    }
    return true;
  }
  void complete_types() override {
    for (auto &expr : *this) {
      expr->complete_types();
    }
  }
};
struct namespace_tree : expression_tree {
  token name;
  inline std::ostream &print(std::ostream &out) const override {
    out << "namespace " << name << " contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
  }
};
} // namespace selflang