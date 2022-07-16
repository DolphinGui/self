#pragma once
#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"
#include <cstddef>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
namespace self {
namespace detail {
constexpr auto ctr_lambda = [](auto in, int hash) {
  in.hash = hash;
  return in;
};
enum BuiltinInstruction { none = 0, store, addi, subi, muli, divi, assign };
} // namespace detail
struct TypeType : Type {
  TokenView name;
  TypeType(TokenView n) : name(n) {}
  TokenView getTypename() const noexcept override { return name; }
  bool operator==(const TypeType &other) const noexcept {
    return name == other.name;
  }
};
const inline auto type_inst = TypeType("type");

struct BuiltinTypeLit : public Literal<TypeType> {
  // I really need to create a global object
  static inline std::unordered_map<TokenView, const BuiltinTypeLit *>
      internal_type_map;
  BuiltinTypeLit(TypeType val) : Literal(val, TypeRef{type_inst}) {
    internal_type_map.insert({this->value.name, this});
  }
  static bool contains(TokenView t) { return internal_type_map.contains(t); }
  static BuiltinTypeLit get(TokenView t) { return *internal_type_map.at(t); }
};
const inline auto type_t = BuiltinTypeLit(TypeType("type"));
const inline auto i64_t = BuiltinTypeLit(TypeType("i64"));
const inline auto char_t = BuiltinTypeLit(TypeType("char"));
const inline auto f64_t = BuiltinTypeLit(TypeType("f64"));
const inline auto void_t = BuiltinTypeLit(TypeType("void"));

const inline auto i64_assignment = detail::ctr_lambda(
    OperatorDef("=", i64_t.value,
                VarDeclarationPtr("this", TypeRef{i64_t.value, RefTypes::ref}),
                VarDeclarationPtr("RHS", i64_t.value), true),
    detail::store);
const inline auto addi = detail::ctr_lambda(
    OperatorDef("+", i64_t.value, VarDeclarationPtr("LHS", i64_t.value),
                VarDeclarationPtr("RHS", i64_t.value)),
    detail::addi);
const inline auto subi = detail::ctr_lambda(
    OperatorDef("-", i64_t.value, VarDeclarationPtr("LHS", i64_t.value),
                VarDeclarationPtr("RHS", i64_t.value)),
    detail::subi);
const inline auto muli = detail::ctr_lambda(
    OperatorDef("*", i64_t.value, VarDeclarationPtr("LHS", i64_t.value),
                VarDeclarationPtr("RHS", i64_t.value)),
    detail::muli);
const inline auto divi = detail::ctr_lambda(
    OperatorDef("/", i64_t.value, VarDeclarationPtr("LHS", i64_t.value),
                VarDeclarationPtr("RHS", i64_t.value)),
    detail::divi);
const inline auto struct_assignment =
    OperatorDef("=", TypeRef(type_inst, RefTypes::ref),
                VarDeclarationPtr("this", TypeRef(type_inst, RefTypes::ref)),
                VarDeclarationPtr("RHS", type_inst), true);

// using int_literal = literal<size_t>;
struct IntLit : public Literal<size_t> {
  IntLit(size_t val) : Literal(val, i64_t.value) {}
};
struct CharLit : public Literal<unsigned char> {
  CharLit(unsigned char val) : Literal(val, char_t.value) {}
};
struct FloatLit : public Literal<double> {
  FloatLit(double val) : Literal(val, f64_t.value) {}
};
struct StructLit : public Literal<StructDef> {
  StructLit(StructDef &&val) : Literal(std::move(val), type_inst) {}
};
struct OpaqueLit : public Literal<OpaqueStruct> {
  OpaqueLit(OpaqueStruct &&val) : Literal(std::move(val), type_inst) {}
};

inline TypeRef getLiteralType(const Expression &e) {
  if (auto *builtin = dynamic_cast<const BuiltinTypeLit *>(&e)) {
    return builtin->value;
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