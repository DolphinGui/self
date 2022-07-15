#pragma once

#include "ast/expression.hpp"
#include <functional>
#include <unordered_set>

namespace self {

class VarDeclaration : public Expression {
  Token name;

public:
  TypePtr type_ref;
  VarDeclaration(TokenView name) : name(name), type_ref{nullptr} {}
  VarDeclaration(TokenView name, TypeRef type)
      : name(name), type_ref{&type.ptr, type.is_ref} {}

  TokenView getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    if (type_ref.ptr)
      return out << "var " << name << ": " << type_ref.ptr->getTypename();
    else
      return out << "var  " << name;
  }
  bool isComplete() const override { return type_ref.ptr; }
  TypePtr getType() const noexcept override {
    auto result = type_ref;
    result.is_ref = RefTypes::ref;
    return result;
  }
  TypePtr getDecltype() const noexcept { return type_ref; }
};

// name, type/value
std::unique_ptr<VarDeclaration> VarDeclarationPtr(auto &&...args) {
  return std::make_unique<VarDeclaration>(
      std::forward<decltype(args)>(args)...);
}

class VarDeref : public Expression {
  const VarDeclaration &definition;

public:
  VarDeref(const VarDeclaration &itself) : definition(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable dereference: " << definition;
  }
  inline std::string_view getName() const noexcept override {
    return "var ref";
  }
  virtual TypePtr getType() const noexcept override {
    auto type = definition.getType();
    type.is_ref = RefTypes::ref;
    return type;
  }
};
} // namespace self