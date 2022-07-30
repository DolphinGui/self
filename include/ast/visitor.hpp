#pragma once

namespace self {
struct ExprBase;
struct Ret;
struct If;
struct While;
struct ExprTree;
struct FunBase;
struct StructDef;
struct Tuple;
class UnevaluatedExpression;
class VarDeclaration;
struct VarDeref;
struct BuiltinTypeLit;
struct IntLit;
struct FloatLit;
struct StringLit;
struct BoolLit;
struct StructLit;
// struct Literal
struct ExprVisitor {
  virtual void operator()(const ExprBase &) const {};
  virtual void operator()(const Ret &) const {}
  virtual void operator()(const If &) const {}
  virtual void operator()(const While &) const {}
  virtual void operator()(const ExprTree &) const {}
  virtual void operator()(const FunBase &) const {}
  virtual void operator()(const StructDef &) const {}
  virtual void operator()(const Tuple &) const {}
  virtual void operator()(const UnevaluatedExpression &) const {}
  virtual void operator()(const VarDeclaration &) const {}
  virtual void operator()(const VarDeref &) const {}
  virtual void operator()(const BuiltinTypeLit &) const {}
  virtual void operator()(const IntLit &) const {}
  virtual void operator()(const FloatLit &) const {}
  virtual void operator()(const StringLit &) const {}
  virtual void operator()(const BoolLit &) const {}
  virtual void operator()(const StructLit &) const {}
};
} // namespace self