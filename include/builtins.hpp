#pragma once
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"

#include <algorithm>
#include <cstddef>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <iterator>
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
  TypeType(TypeType &&) = delete;
  TypeType(const TypeType &) = delete;
  TokenView getTypename() const noexcept override { return name; }
  bool operator==(const TypeType &other) const noexcept {
    return name == other.name;
  }
};
using BuiltinTypeRef = std::reference_wrapper<const Type>;
inline auto operator<<(std::ostream &out, BuiltinTypeRef in) {
  out << in.get().getTypename();
};

struct Context {
  std::unordered_map<Token, BuiltinTypeRef> internal_type_map;
  Index root;
  std::vector<StructDef> foreign_types;
  Context() : root(Index::createRoot()) {
    for (const auto &type : std::initializer_list<BuiltinTypeRef>{
             type_t, i64_t, f64_t, void_t, str_t, bool_t, u64_t}) {
      internal_type_map.insert(
          {Token(type.get().getTypename()), std::cref(type)});
    }
    for (auto op : std::initializer_list<ExprConstRef>{
             i64_assignment, addi, subi, muli, divi, cmpi, struct_assignment,
             str_assignment}) {
      root.insert({op.get().getName(), op});
    }
  }
  bool insertForeignType(StructDef &&type) {
    foreign_types.push_back(std::move(type));
    return internal_type_map
        .insert({Token(foreign_types.back().identity),
                 std::cref(static_cast<const Type &>(foreign_types.back()))})
        .second;
  }
  Context(const Context &) = delete;
  const TypeType type_t = TypeType("type");
  const TypeType i64_t = TypeType("i64");
  const TypeType u64_t = TypeType("u64");
  const TypeType f64_t = TypeType("f64");
  const TypeType void_t = TypeType("void");
  const TypeType str_t = TypeType("str");
  const TypeType bool_t = TypeType("bool");

  const OperatorDef i64_assignment = OperatorDef(
      "=", i64_t, VarDeclarationPtr("this", TypeRef{i64_t, RefTypes::ref}),
      VarDeclarationPtr("RHS", i64_t), root, detail::store, true);
  const OperatorDef addi =
      OperatorDef("+", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::addi);
  const OperatorDef subi =
      OperatorDef("-", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::subi);
  const OperatorDef muli =
      OperatorDef("*", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::muli);
  const OperatorDef divi =
      OperatorDef("/", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::divi);
  const OperatorDef cmpi =
      OperatorDef("==", bool_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::cmp);
  const OperatorDef struct_assignment =
      OperatorDef("=", TypeRef(type_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(type_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", type_t), root, detail::assign, true);
  const OperatorDef str_assignment =
      OperatorDef("=", TypeRef(str_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(str_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", str_t), root, detail::store, true);
};

struct Module {
  Module(Index &&i, ExprTree &&tree)
      : global_context(std::move(i)), ast(std::move(tree)) {}
  Index global_context;
  ExprTree ast;
};

struct BuiltinTypeLit : public LiteralImpl<BuiltinTypeRef, BuiltinTypeLit> {
  // I really need to create a global object
  BuiltinTypeLit(BuiltinTypeRef val, Context &c) : LiteralImpl(val, c.type_t) {}

  static bool contains(TokenView t, Context &c) {
    return c.internal_type_map.contains(Token(t));
  }
  static BuiltinTypeLit get(TokenView t, Context &c) {
    return BuiltinTypeLit(c.internal_type_map.at(Token(t)), c);
  }

  inline void printval(std::ostream &out) const {
    out << value.get().getTypename();
  }
};
struct IntLit : public LiteralImpl<int64_t, IntLit> {
  IntLit(int64_t val, Context &c) : LiteralImpl(val, c.i64_t) {}
};
struct FloatLit : public LiteralImpl<double, FloatLit> {
  FloatLit(double val, Context &c) : LiteralImpl(val, c.f64_t) {}
};
struct StringLit : public LiteralImpl<std::vector<unsigned char>, StringLit> {
  StringLit(std::vector<unsigned char> s, Context &c)
      : LiteralImpl(s, c.str_t) {}

  inline void printval(std::ostream &out) const {
    for (char c : value) {
      out << c;
    }
  }

  explicit operator std::string() const {
    std::string result;
    result.reserve(value.size());
    std::copy(value.cbegin(), value.cend(), std::back_inserter(result));
    return result;
  }

  inline TokenView getName() const noexcept override {
    return "Literal string";
  }
};

struct BoolLit : public LiteralImpl<bool, BoolLit> {
  BoolLit(bool val, Context &c) : LiteralImpl(val, c.i64_t) {}
};

struct StructLit : public LiteralImpl<StructDef, StructLit> {
  StructLit(StructDef &&val, Context &c)
      : LiteralImpl(std::move(val), c.type_t) {}
};
struct OpaqueLit : public LiteralImpl<OpaqueStruct, OpaqueLit> {
  OpaqueLit(OpaqueStruct &&val, Context &c)
      : LiteralImpl(std::move(val), c.type_t) {}
};

// using StringLit = LiteralImpl<std::vector<unsigned char>>;
inline TypeRef getLiteralType(const ExprBase &e) {
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