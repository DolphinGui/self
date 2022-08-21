#pragma once

#include <algorithm>
#include <cstddef>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"
#include "ast/visitor.hpp"

namespace self {
struct StructDef : public ExprImpl<StructDef>, Type {
  std::string identity;
  std::unique_ptr<Index> context;
  ExprTree body;
  size_t opaque_size = std::numeric_limits<size_t>::max();
  bool is_opaque = false;

  StructDef(Index &parent) : context(std::make_unique<Index>(parent)) {}
  StructDef(size_t size, Index &parent)
      : context(std::make_unique<Index>(parent)), opaque_size(size),
        is_opaque(true) {}
  StructDef(StructDef &&other) = default;
  StructDef(const StructDef &other)
      : identity(other.identity),
        context(std::make_unique<Index>(*other.context)),
        opaque_size(other.opaque_size), is_opaque(other.is_opaque) {
    std::for_each(other.body.cbegin(), other.body.cend(),
                  [&](const ExprPtr &e) { body.push_back(e->clone()); });
  }
  void insert(auto &&in) {
    body.push_back(std::move(in));
    context->insert({body.back()->getName(), *body.back()});
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
struct MemberDeref : public ExprImpl<MemberDeref> {
  // the member being dereferenced
  VarDeclaration &definition;
  // the dereferenced structure
  std::unique_ptr<VarDeref> structure;
  const StructDef &type;

  MemberDeref(VarDeclaration &definition, std::unique_ptr<VarDeref> &&structure,
              const StructDef &type)
      : definition(definition), structure(std::move(structure)), type(type) {}

  MemberDeref(const MemberDeref &other)
      : definition(other.definition),
        structure(std::make_unique<VarDeref>(*other.structure)),
        type(other.type) {}

  inline std::ostream &print(std::ostream &out) const override {
    return out << "member dereference: " << definition;
  }

  inline std::string_view getName() const noexcept override {
    return definition.getName();
  }

  virtual TypePtr getType() const noexcept override {
    return definition.getType();
  }
};
} // namespace self