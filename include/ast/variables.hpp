#pragma once

#include "ast/expression.hpp"
#include "ast/visitor.hpp"
#include <cstring>
#include <functional>
#include <memory>
#include <unordered_set>
#include <vector>

namespace self {

class VarDeclaration : public ExprImpl<VarDeclaration>,
                       public NameQualification<VarDeclaration> {
  Token name;

public:
  TypePtr type_ref;
  ExprBase *value = nullptr;
  VarDeclaration(TokenView name) : name(qualify(name)), type_ref{nullptr} {}
  VarDeclaration(TokenView name, TypeRef type)
      : name(qualify(name)), type_ref{&type.ptr, type.is_ref} {}

  inline std::ostream &print(std::ostream &out) const override {
    if (type_ref.ptr)
      return out << "var " << dequalify(name) << ": " << type_ref.dump();
    else
      return out << "var  " << dequalify(name);
  }

  TypePtr getType() const noexcept override {
    auto result = type_ref;
    result.is_ref = RefTypes::ref;
    ++result.depth;
    return result;
  }

  static constexpr auto prefix = "__var_";

  TypePtr getDecltype() const noexcept { return type_ref; }
  bool isComplete() const override { return type_ref.ptr; }
  TokenView getName() const noexcept override { return name; }
};

// name, type/value
std::unique_ptr<VarDeclaration> VarDeclarationPtr(auto &&...args) {
  return std::make_unique<VarDeclaration>(
      std::forward<decltype(args)>(args)...);
}

struct VarDeref : public ExprImpl<VarDeref> {
  VarDeclaration &definition;
  std::string_view name;

  VarDeref(VarDeclaration &definition)
      : definition(definition), name(definition.getName()) {}

  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable dereference: " << definition;
  }

  inline std::string_view getName() const noexcept override { return name; }

  virtual TypePtr getType() const noexcept override {
    return definition.getType();
  }
};
} // namespace self