#pragma once

#include "ast/expression.hpp"
#include <cstring>
#include <functional>
#include <memory>
#include <unordered_set>

namespace self {

class VarDeclaration : public ExprImpl<VarDeclaration>,
                       public NameMangling<VarDeclaration> {
  Token name;

public:
  TypePtr type_ref;
  ExprBase *value;
  VarDeclaration(TokenView name) : name(mangle(name)), type_ref{nullptr} {}
  VarDeclaration(TokenView name, TypeRef type)
      : name(mangle(name)), type_ref{&type.ptr, type.is_ref} {}

  inline std::ostream &print(std::ostream &out) const override {
    if (type_ref.ptr)
      return out << "var " << demangle(name) << ": "
                 << type_ref.ptr->getTypename();
    else
      return out << "var  " << demangle(name);
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
  const VarDeclaration &definition;
  std::string name;

  VarDeref(const VarDeclaration &definition)
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