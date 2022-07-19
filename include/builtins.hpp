#pragma once
#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"

#include <algorithm>
#include <cstddef>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>
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
    for (const auto &type : std::initializer_list<BuiltinTypeRef>{
             type_t, i64_t, char_t, f64_t, void_t}) {
      internal_type_map.insert({type.get().getTypename(), std::cref(type)});
    }
  };
  Context(const Context &) = delete;
  const TypeType type_t = TypeType("type");
  const TypeType i64_t = TypeType("i64");
  const TypeType char_t = TypeType("char");
  const TypeType f64_t = TypeType("f64");
  const TypeType void_t = TypeType("void");
  const TypeType str_t = TypeType("str");
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
struct BuiltinTypeLit : public LiteralImpl<BuiltinTypeRef, BuiltinTypeLit> {
  // I really need to create a global object
  BuiltinTypeLit(BuiltinTypeRef val, Context &c) : LiteralImpl(val, c.type_t) {}

  static bool contains(TokenView t, Context &c) {
    return c.internal_type_map.contains(t);
  }
  static BuiltinTypeLit get(TokenView t, Context &c) {
    auto b = c.internal_type_map.at(t).get();
    auto result = BuiltinTypeLit(c.internal_type_map.at(t), c);
    return result;
  }

  ExpressionPtr clone() const override {
    return std::make_unique<BuiltinTypeLit>(*this);
  }
  inline void printval(std::ostream &out) const {
    out << value.get().getTypename();
  }
};
struct IntLit : public LiteralImpl<size_t, IntLit> {
  IntLit(size_t val, Context &c) : LiteralImpl(val, c.i64_t) {}
};
struct CharLit : public LiteralImpl<unsigned char, CharLit> {
  CharLit(unsigned char val, Context &c) : LiteralImpl(val, c.char_t) {}
  inline void printval(std::ostream &out) const {
    out << '\'' << value << '\'';
  }
};
struct FloatLit : public LiteralImpl<double, FloatLit> {
  FloatLit(double val, Context &c) : LiteralImpl(val, c.f64_t) {}
};
struct StructLit : public LiteralImpl<StructDef, StructLit> {
  StructLit(StructDef &&val, Context &c)
      : LiteralImpl(std::move(val), c.type_t) {}
};
struct OpaqueLit : public LiteralImpl<OpaqueStruct, OpaqueLit> {
  OpaqueLit(OpaqueStruct &&val, Context &c)
      : LiteralImpl(std::move(val), c.type_t) {}
};

struct StringLit : public LiteralImpl<std::vector<unsigned char>, StringLit> {
  StringLit(std::string_view s, Context &c)
      : LiteralImpl(string_converter(s), c.str_t) {}

  inline void printval(std::ostream &out) const {
    for (char c : value) {
      out << c;
    }
  }

  inline TokenView getName() const noexcept override {
    return "Literal string";
  };

private:
  static constexpr auto string_converter =
      [](std::string_view s) -> std::vector<unsigned char> {
    std::vector<unsigned char> result;
    result.reserve(s.size());
    std::for_each(s.cbegin(), s.cend(), [&](char c) { result.push_back(c); });
    return result;
  };
};
// using StringLit = LiteralImpl<std::vector<unsigned char>>;
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

} // namespace self