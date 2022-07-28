#pragma once
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"
#include "error_handling.hpp"

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
    for (auto op : std::initializer_list<ExprRef>{
             i64_assignment, addi, subi, muli, divi, eqi, neqi,
             struct_assignment, str_assignment, bool_assignment, ref_cast,
             ref_assignment}) {
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
  const TypeType deduced_t = TypeType("deduced");
  const TypeType i64_t = TypeType("i64");
  const TypeType u64_t = TypeType("u64");
  const TypeType f64_t = TypeType("f64");
  const TypeType void_t = TypeType("void");
  const TypeType str_t = TypeType("str");
  const TypeType bool_t = TypeType("bool");

  OperatorDef i64_assignment = OperatorDef(
      "=", i64_t, VarDeclarationPtr("this", TypeRef{i64_t, RefTypes::ref}),
      VarDeclarationPtr("RHS", i64_t), root, detail::store, true);
  OperatorDef addi =
      OperatorDef("+", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::addi);
  OperatorDef subi =
      OperatorDef("-", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::subi);
  OperatorDef muli =
      OperatorDef("*", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::muli);
  OperatorDef divi =
      OperatorDef("/", i64_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::divi);
  OperatorDef eqi =
      OperatorDef("==", bool_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::cmpeq);
  OperatorDef neqi =
      OperatorDef("!=", bool_t, VarDeclarationPtr("LHS", i64_t),
                  VarDeclarationPtr("RHS", i64_t), root, detail::cmpneq);
  OperatorDef struct_assignment =
      OperatorDef("=", TypeRef(type_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(type_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", type_t), root, detail::assign, true);
  OperatorDef str_assignment =
      OperatorDef("=", TypeRef(str_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(str_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", str_t), root, detail::store, true);
  OperatorDef bool_assignment =
      OperatorDef("=", TypeRef(bool_t, RefTypes::ref),
                  VarDeclarationPtr("this", TypeRef(bool_t, RefTypes::ref)),
                  VarDeclarationPtr("RHS", bool_t), root, detail::store, true);
  FunctionDef ref_cast = FunctionDef(
      detail::addr, "ref", root, TypeRef(deduced_t, RefTypes::ref, 2), false,
      VarDeclarationPtr("arg", TypeRef(deduced_t, RefTypes::ref)));
  OperatorDef ref_assignment = OperatorDef(
      "=", TypeRef(deduced_t, RefTypes::ref),
      VarDeclarationPtr("this", TypeRef(deduced_t, RefTypes::ref)),
      VarDeclarationPtr("RHS", TypeRef(deduced_t, RefTypes::ref, 2)), root,
      detail::store, true);
};

struct Module {
  Module(Index &&i, ExprTree &&tree, ErrorList &&errs)
      : global_context(std::move(i)), ast(std::move(tree)),
        errs(std::move(errs)) {}
  Index global_context;
  ExprTree ast;
  ErrorList errs;
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
  BoolLit(bool val, Context &c) : LiteralImpl(val, c.bool_t) {}
  inline void printval(std::ostream &out) const {
    if (value)
      out << "true";
    else
      out << "false";
  }
};

struct StructLit : public LiteralImpl<StructDef, StructLit> {
  StructLit(StructDef &&val, Context &c)
      : LiteralImpl(std::move(val), c.type_t) {}
  inline void printval(std::ostream &out) const { out << value; }
};
// using StringLit = LiteralImpl<std::vector<unsigned char>>;
inline TypeRef getLiteralType(const ExprBase &e) {
  if (auto *builtin = dynamic_cast<const BuiltinTypeLit *>(&e)) {
    return builtin->value.get();
  } else if (auto *structural = dynamic_cast<const StructLit *>(&e)) {
    return structural->value;
  } else
    throw std::runtime_error("That's not a type expression");
}

} // namespace self