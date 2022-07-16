#pragma once
#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"

#include <cstddef>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
namespace self {
namespace detail {
constexpr auto ctr_lambda = [](auto &&in, detail::BuiltinInstruction hash) {
  in.internal = hash;
  return in;
};
} // namespace detail
struct TypeType : Type {
  TokenView name;
  TypeType(TokenView n) : name(n) {}
  TokenView getTypename() const noexcept override { return name; }
  bool operator==(const TypeType &other) const noexcept {
    return name == other.name;
  }
};
using BuiltinTypeRef = std::reference_wrapper<const TypeType>;
inline auto operator<<(std::ostream &out, BuiltinTypeRef in) {
  out << in.get().getTypename();
};
struct Context {
  Context() {
    for (auto &a : {type_t, i64_t, char_t, f64_t, void_t}) {
      internal_type_map.insert({a.getTypename(), std::cref(type_t)});
    }
  };
  Context(const Context &) = delete;
  const TypeType type_t = TypeType("type");
  const TypeType i64_t = TypeType("i64");
  const TypeType char_t = TypeType("char");
  const TypeType f64_t = TypeType("f64");
  const TypeType void_t = TypeType("void");
  std::unordered_map<TokenView, BuiltinTypeRef> internal_type_map;

  const OperatorDef i64_assignment = detail::ctr_lambda(
      OperatorDef("=", i64_t,
                  VarDeclarationPtr("this", TypeRef{i64_t, RefTypes::ref}),
                  VarDeclarationPtr("RHS", i64_t), true),
      detail::store);
  const OperatorDef addi = detail::ctr_lambda(
      OperatorDef("+", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t)),
      detail::addi);
  const OperatorDef subi = detail::ctr_lambda(
      OperatorDef("-", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t)),
      detail::subi);
  const OperatorDef muli = detail::ctr_lambda(
      OperatorDef("*", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t)),
      detail::muli);
  const OperatorDef divi = detail::ctr_lambda(
      OperatorDef("/", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t)),
      detail::divi);
  const OperatorDef struct_assignment =
      OperatorDef("=", TypeRef(type_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(type_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", type_t), true);
};
struct BuiltinTypeLit : public Literal<BuiltinTypeRef> {
  // I really need to create a global object
  BuiltinTypeLit(BuiltinTypeRef val, Context &c) : Literal(val, c.type_t) {}

  static bool contains(TokenView t, Context &c) {
    return c.internal_type_map.contains(t);
  }
  static BuiltinTypeLit get(TokenView t, Context &c) {
    return {c.internal_type_map.at(t), c};
  }
};
struct IntLit : public Literal<size_t> {
  IntLit(size_t val, Context &c) : Literal(val, c.i64_t) {}
};
struct CharLit : public Literal<unsigned char> {
  CharLit(unsigned char val, Context &c) : Literal(val, c.char_t) {}
};
struct FloatLit : public Literal<double> {
  FloatLit(double val, Context &c) : Literal(val, c.f64_t) {}
};
struct StructLit : public Literal<StructDef> {
  StructLit(StructDef &&val, Context &c) : Literal(std::move(val), c.type_t) {}
};
struct OpaqueLit : public Literal<OpaqueStruct> {
  OpaqueLit(OpaqueStruct &&val, Context &c)
      : Literal(std::move(val), c.type_t) {}
};

inline TypeRef getLiteralType(const Expression &e) {
  if (auto *builtin = dynamic_cast<const BuiltinTypeLit *>(&e)) {
    return builtin->value.get();
  } else if (auto *structural = dynamic_cast<const StructLit *>(&e)) {
    return structural->value;
  } else if (auto *opaque = dynamic_cast<const OpaqueLit *>(&e)) {
    return opaque->value;
  } else
    throw std::runtime_error("That's not a type expression");
}

using TokenView = std::string_view;
template <> struct Literal<std::vector<unsigned char>> : public Expression {
  std::vector<unsigned char> value;
  Literal(std::vector<unsigned char> itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    out << "Literal: \"";
    for (char c : value) {
      out << c;
    }
    return out << '\"';
  }
  inline TokenView getName() const noexcept override {
    return "Literal value";
  };
};

using StringLit = Literal<std::vector<unsigned char>>;

} // namespace self