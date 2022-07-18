#pragma once

#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"

namespace self {
struct StructDef : public Expression, Type {
  std::string identity;
  ExpressionTree body;

  StructDef() = default;
  StructDef(StructDef &&) = default;
  StructDef(const StructDef &other) : identity(other.identity) {
    std::for_each(other.body.cbegin(), other.body.cend(),
                  [&](const ExpressionPtr &e) { body.push_back(e->clone()); });
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

  ExpressionPtr clone() const override {
    return std::make_unique<StructDef>(*this);
  }
};
struct OpaqueStruct : public Expression, Type {
  OpaqueStruct(unsigned int identity, size_t size = 0)
      : size(size),
        identity(std::string("struct").append(std::to_string(identity))) {}
  size_t size = 0;
  std::string identity;
  std::ostream &print(std::ostream &out) const override {
    out << "opaque struct" << size;
    if (size)
      out << " size " << size;
    return out;
  }
  TokenView getName() const noexcept override { return "opaque struct"; }
  TokenView getTypename() const noexcept override { return identity; }
  ExpressionPtr clone() const override {
    return std::make_unique<OpaqueStruct>(*this);
  }
};
} // namespace self