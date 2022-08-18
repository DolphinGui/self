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
struct MemberDeref;
class FunctionCall;
// struct Literal
struct ExprVisitor {
  // I hate having to do this
  // but it was the only way to
  // have a generalized visitor
  // without having to involve
  // the return values and other passed
  // parameters intrusively.
  virtual void operator()(const ExprBase &, void *){};
  virtual void operator()(const Ret &, void *) {}
  virtual void operator()(const If &, void *) {}
  virtual void operator()(const While &, void *) {}
  virtual void operator()(const ExprTree &, void *) {}
  virtual void operator()(const FunBase &, void *) {}
  virtual void operator()(const StructDef &, void *) {}
  virtual void operator()(const Tuple &, void *) {}
  virtual void operator()(const UnevaluatedExpression &, void *) {}
  virtual void operator()(const VarDeclaration &, void *) {}
  virtual void operator()(const VarDeref &, void *) {}
  virtual void operator()(const BuiltinTypeLit &, void *) {}
  virtual void operator()(const IntLit &, void *) {}
  virtual void operator()(const FloatLit &, void *) {}
  virtual void operator()(const StringLit &, void *) {}
  virtual void operator()(const BoolLit &, void *) {}
  virtual void operator()(const StructLit &, void *) {}
  virtual void operator()(const FunctionCall &, void *) {}
  virtual void operator()(const MemberDeref&, void*){}
};
} // namespace self