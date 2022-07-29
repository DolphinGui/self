#pragma once

namespace self {
struct Ret;
struct If;
struct While;
struct ExprTree;
struct FunBase;
struct StructDef;
struct Tuple;
struct UnevaluatedExpression;
struct VarDeclaration;
struct VarDeref;
struct BuiltinTypeLit;
struct IntLit;
struct FloatLit;
struct StringLit;
struct BoolLit;
struct StructLit;
// struct Literal
struct ExprVisitor {
  virtual void operator()(const Ret &) const = 0;
  virtual void operator()(const If &) const = 0;
  virtual void operator()(const While &) const = 0;
  virtual void operator()(const ExprTree &) const = 0;
  virtual void operator()(const FunBase &) const = 0;
  virtual void operator()(const StructDef &) const = 0;
  virtual void operator()(const Tuple &) const = 0;
  virtual void operator()(const UnevaluatedExpression &) const = 0;
  virtual void operator()(const VarDeclaration &) const = 0;
  virtual void operator()(const VarDeref &) const = 0;
  virtual void operator()(const BuiltinTypeLit &) const = 0;
  virtual void operator()(const IntLit &) const = 0;
  virtual void operator()(const FloatLit &) const = 0;
  virtual void operator()(const StringLit &) const = 0;
  virtual void operator()(const BoolLit &) const = 0;
  virtual void operator()(const StructLit &) const = 0;
};
} // namespace self