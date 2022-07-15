#pragma once
#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"
#include <cstddef>
#include <functional>
#include <string_view>
namespace self {
namespace detail {
constexpr auto ctr_lambda = [](auto in, int hash) {
  in.hash = hash;
  return in;
};
enum BuiltinInstruction { none = 0, store, addi, subi, muli, divi };
} // namespace detail
struct TypeType : Type {
  TokenView name;
  TypeType(TokenView n) : name(n) {}
  TokenView getTypename() const noexcept override { return name; }
};
const inline auto type_inst = TypeType("type");
const inline auto i64_t =
    Literal<TypeType>(TypeType("i64"), TypeRef{type_inst});
const inline auto char_t =
    Literal<TypeType>(TypeType("char"), TypeRef{type_inst});
const inline auto f64_t =
    Literal<TypeType>(TypeType("f64"), TypeRef{type_inst});
const inline auto void_t =
    Literal<TypeType>(TypeType("void"), TypeRef{type_inst});

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