#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"
#include "ast/visitor.hpp"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <optional>
#include <string_view>

namespace self {

namespace detail {

enum BuiltinInstruction {
  call = 0,
  store,
  addi,
  subi,
  muli,
  divi,
  cmpeq,
  cmpneq,
  assign,
  addr,
  assignaddr
};
inline void iterate(auto lambda, auto &&arg) { lambda(arg); }
inline void iterate(auto lambda, auto &&arg, auto &&...args) {
  lambda(arg);
  iterate(lambda, args...);
}
} // namespace detail

struct FunBase : public ExprBase {
  Token name;
  Token foreign_name;
  std::optional<Block> body;
  TypePtr return_type = {nullptr};
  detail::BuiltinInstruction internal;
  bool member = false;
  bool body_defined = false;
  virtual Token getDemangled() const = 0;

protected:
  FunBase(TokenView name, Index &parent, bool member = false)
      : name(name), body(parent), internal(detail::call), member(member) {
    if (name == "__function_main") {
      foreign_name = "main";
    }
  }

  FunBase(TokenView name, TypeRef return_type, Index &parent,
          bool member = false,
          detail::BuiltinInstruction internal = detail::call)
      : name(name), body(parent),
        return_type(&return_type.ptr, return_type.is_ref), internal(internal),
        member(member) {
    if (name == "__function_main") {
      foreign_name = "main";
    }
  }

  FunBase(const FunBase &other)
      : name(other.name), body(other.body), return_type(other.return_type),
        internal(other.internal), member(other.member) {}

public:
  std::string_view getName() const noexcept override { return name; }
  bool isComplete() const noexcept override { return return_type.ptr; }
  virtual std::size_t argcount() const noexcept = 0;
  std::string_view getForeignName() const noexcept {
    if (foreign_name.empty())
      return getName();
    else
      return foreign_name;
  }
};

class OperatorDef : public FunBase, public NameMangling<OperatorDef> {
public:
  std::unique_ptr<VarDeclaration> lhs;
  std::unique_ptr<VarDeclaration> rhs;
  OperatorDef(TokenView name, TypeRef return_type,
              std::unique_ptr<VarDeclaration> &&LHS,
              std::unique_ptr<VarDeclaration> &&RHS, Index &parent,
              detail::BuiltinInstruction internal = detail::call,
              bool member = false)
      : FunBase(mangle(name), return_type, parent, member, internal),
        lhs(std::move(LHS)), rhs(std::move(RHS)) {}
  OperatorDef(const OperatorDef &other)
      : FunBase(other), lhs(std::make_unique<VarDeclaration>(*other.lhs)),
        rhs(std::make_unique<VarDeclaration>(*other.rhs)) {}
  std::size_t argcount() const noexcept override {
    if (!lhs || !rhs)
      return 1;
    return 2;
  }
  std::ostream &print(std::ostream &out) const override {
    out << "operator " << demangle(name) << "( ";
    if (lhs) {
      out << *lhs << ", ";
    }
    if (rhs) {
      out << *rhs;
    }
    out << ')';
    if (return_type.ptr) {
      out << " -> " << return_type.ptr->getTypename();
    }
    if (body_defined) {
      out << '{' << *body << '}';
    }
    return out;
  }

  ExprPtr clone() const override {
    return std::make_unique<OperatorDef>(*this);
  }
  void visit(ExprVisitor &visitor, void *d) const override {
    visitor(*this, d);
  }
  constexpr static auto prefix = "__operator_";
  Token getDemangled() const override { return getDemangledName(); }
};

struct FunctionDef : public FunBase, public NameMangling<FunctionDef> {
  std::vector<std::unique_ptr<VarDeclaration>> arguments;
  FunctionDef(TokenView name, Index &parent, bool member = false)
      : FunBase(mangle(name), parent, member) {}
  FunctionDef(TokenView name, Index &parent, TypeRef return_type,
              bool member = false, auto &&...args)
      : FunBase(mangle(name), return_type, parent, member, detail::call) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  FunctionDef(TokenView name, Index &parent, TypeRef return_type,
              bool member = false,
              std::vector<std::unique_ptr<VarDeclaration>> &&args = {})
      : FunBase(mangle(name), return_type, parent, member, detail::call),
        arguments(std::move(args)) {}
  FunctionDef(detail::BuiltinInstruction b, TokenView name, Index &parent,
              TypeRef return_type, bool member = false, auto &&...args)
      : FunBase(mangle(name), return_type, parent, member, detail::call) {
    internal = b;
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  FunctionDef(const FunctionDef &other) : FunBase(other) {
    std::for_each(other.arguments.cbegin(), other.arguments.cend(),
                  [&](const auto &o) {
                    arguments.push_back(std::make_unique<VarDeclaration>(*o));
                  });
  }
  ExprPtr clone() const override {
    return std::make_unique<FunctionDef>(*this);
  }
  void visit(ExprVisitor &visitor, void *d) const override {
    visitor(*this, d);
  }
  std::size_t argcount() const noexcept override { return arguments.size(); }
  std::ostream &print(std::ostream &out) const override {
    out << "fun " << demangle(name) << "( ";
    for (const auto &arg : arguments) {
      out << *arg << ", ";
    }
    out << ')';
    if (return_type.ptr) {
      out << " -> " << return_type.ptr->getTypename();
    }
    if (body_defined) {
      out << '{' << *body << '}';
    }
    return out;
  }
  constexpr static auto prefix = "__function_";
  Token getDemangled() const override { return getDemangledName(); }
};

class FunctionCall : public ExprImpl<FunctionCall> {
public:
  const FunBase &definition;
  ExprPtr lhs;
  ExprPtr rhs;
  FunctionCall(const FunBase &Callee) : definition(Callee) {}
  FunctionCall(const FunBase &callee, ExprPtr &&LHS, ExprPtr &&RHS)
      : definition(callee), lhs(std::move(LHS)), rhs(std::move(RHS)) {}
  FunctionCall(const FunctionCall &other)
      : definition(other.definition), lhs(cloneif(other.lhs)),
        rhs(cloneif(other.rhs)) {}
  inline std::ostream &print(std::ostream &out) const override {
    if (lhs)
      out << *lhs << ' ';
    out << definition.getDemangled() << ' ';
    if (rhs)
      out << *rhs;
    return out;
  }
  bool isComplete() const noexcept override {
    if (!lhs && !rhs)
      return false;
    std::size_t count;
    if (!lhs || !rhs)
      count = 1;
    else
      count = 2;
    return count == definition.argcount();
  }
  const FunBase &getDefinition() const noexcept { return definition; }
  TokenView getName() const noexcept override { return "op call"; }
  bool isUnaryPre() const noexcept { return !lhs && rhs; }
  bool isUnaryPost() const noexcept { return lhs && !rhs; }
  bool isBinary() const noexcept { return lhs && rhs; }
  bool isUnary() const noexcept { return !isBinary(); }
  virtual TypePtr getType() const noexcept override {
    // yes this is hardcoded no I can't be bothered
    // to pass a global object to this
    if (definition.return_type.ptr->getTypename() == "deduced") {
      // super hardcoded for ref_assignment rn
      auto r = rhs->getType();
      ++r.depth;
      return r;
    }
    return definition.return_type;
  }
};
} // namespace self