#pragma once

#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

#include "ast/visitor.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"

namespace self {
struct StructDef : public ExprImpl<StructDef>, Type {
  std::string identity;
  Index context;
  ExprTree body;
  size_t opaque_size;
  bool is_opaque;

  StructDef(Index &parent) : context(parent) {}
  StructDef(size_t size, Index &parent)
      : context(parent), opaque_size(size), is_opaque(true) {}
  StructDef(StructDef &&other) = default;
  StructDef(const StructDef &other)
      : identity(other.identity), context(other.context),
        opaque_size(other.opaque_size), is_opaque(other.is_opaque) {
    std::for_each(other.body.cbegin(), other.body.cend(),
                  [&](const ExprPtr &e) { body.push_back(e->clone()); });
  }
  std::ostream &print(std::ostream &out) const override {
    out << "struct decl: ";
    for (auto &m : body) {
      out << *m << ' ';
    }
    return out;
  }
  TokenView getName() const noexcept override { return "struct decl"; }
  TokenView getTypename() const noexcept override { return identity; }
};
} // namespace self