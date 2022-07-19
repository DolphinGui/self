#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"
#include <algorithm>
#include <cstddef>
#include <memory>
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
  cmp,
  assign
};
inline void iterate(auto lambda, auto &&arg) { lambda(arg); }
inline void iterate(auto lambda, auto &&arg, auto &&...args) {
  lambda(arg);
  iterate(lambda, args...);
}
} // namespace detail

struct FunBase : public ExprBase {
  Token name;
  ExprTree body;
  TypePtr return_type = {nullptr};
  detail::BuiltinInstruction internal;
  bool member = false;
  bool body_defined = false;

protected:
  FunBase(TokenView name, bool member = false)
      : name(name), internal(detail::call), member(member) {}

  FunBase(TokenView name, TypeRef return_type, bool member = false,
          detail::BuiltinInstruction internal = detail::call)
      : name(name), return_type(&return_type.ptr, return_type.is_ref),
        internal(internal), member(member) {}

public:
  std::string_view getName() const noexcept override { return name; }
  bool isComplete() const noexcept override { return return_type.ptr; }
  virtual std::size_t argcount() const noexcept = 0;
};

class OperatorDef : public FunBase, public NameMangling<OperatorDef> {
public:
  std::unique_ptr<VarDeclaration> lhs;
  std::unique_ptr<VarDeclaration> rhs;
  OperatorDef(TokenView name, TypeRef return_type,
              std::unique_ptr<VarDeclaration> &&LHS,
              std::unique_ptr<VarDeclaration> &&RHS,
              detail::BuiltinInstruction internal = detail::call,
              bool member = false)
      : FunBase(mangle(name), return_type, member, internal),
        lhs(std::move(LHS)), rhs(std::move(RHS)) {}
  OperatorDef(const OperatorDef &other)
      : FunBase(other.name, other.return_type, other.member),
        lhs(std::make_unique<VarDeclaration>(*other.lhs)),
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
      out << '{' << body << '}';
    }
    return out;
  }

  ExprPtr clone() const override {
    return std::make_unique<OperatorDef>(*this);
  }

  constexpr static auto prefix = "__operator_";
};

struct FunctionDef : public FunBase, public NameMangling<FunctionDef> {
  std::vector<std::unique_ptr<VarDeclaration>> arguments;
  FunctionDef(TokenView name, bool member = false)
      : FunBase(mangle(name), member) {}
  FunctionDef(TokenView name, TypeRef return_type, bool member = false,
              auto &&...args)
      : FunBase(mangle(name), return_type, member, detail::call) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  FunctionDef(const FunctionDef &other)
      : FunBase(other.getName(), other.member) {
    std::for_each(other.arguments.cbegin(), other.arguments.cend(),
                  [&](const auto &o) {
                    arguments.push_back(std::make_unique<VarDeclaration>(*o));
                  });
  }
  ExprPtr clone() const override {
    return std::make_unique<FunctionDef>(*this);
  }
  std::size_t argcount() const noexcept override { return arguments.size(); }
  std::ostream &print(std::ostream &out) const override {
    out << "operator " << demangle(name) << "( ";
    for (const auto &arg : arguments) {
      out << *arg << ", ";
    }
    out << ')';
    if (return_type.ptr) {
      out << " -> " << return_type.ptr->getTypename();
    }
    if (body_defined) {
      out << '{' << body << '}';
    }
    return out;
  }
  constexpr static auto prefix = "__function_";
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
      : definition(other.definition), lhs(other.lhs->clone()),
        rhs(other.rhs->clone()) {}
  inline std::ostream &print(std::ostream &out) const override {
    if (lhs)
      out << *lhs << ' ';
    out << definition.getName() << ' ';
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
    return definition.return_type;
  }
};
} // namespace self