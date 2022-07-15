#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"
#include <cstddef>
#include <memory>

namespace self {

namespace detail {
inline void iterate(auto lambda, auto &&arg) { lambda(arg); }
inline void iterate(auto lambda, auto &&arg, auto &&...args) {
  lambda(arg);
  iterate(lambda, args...);
}
} // namespace detail

struct FunBase : public Expression {
  Token name;
  ExpressionTree body;
  TypePtr return_type = {nullptr};
  int hash = 0;
  bool member = false;
  bool body_defined = false;

protected:
  FunBase(TokenView name, bool member = false) : name(name), member(member) {}

  FunBase(TokenView name, TypeRef return_type, bool member = false)
      : name(name), return_type(&return_type.ptr, return_type.is_ref),
        member(member) {}

public:
  // virtual std::ostream &print(std::ostream &out) const override;
  std::string_view getName() const noexcept override { return name; }
  bool isComplete() const noexcept override { return return_type.ptr; }
  virtual std::size_t argcount() const noexcept = 0;
};

class OperatorDef : public FunBase {
public:
  std::unique_ptr<VarDeclaration> LHS;
  std::unique_ptr<VarDeclaration> RHS;
  OperatorDef(TokenView name, TypeRef return_type,
              std::unique_ptr<VarDeclaration> &&LHS,
              std::unique_ptr<VarDeclaration> &&RHS, bool member = false)
      : FunBase(name, return_type, member), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  std::size_t argcount() const noexcept override {
    if (!LHS || !RHS)
      return 1;
    return 2;
  }
  std::ostream &print(std::ostream &out) const override {
    out << "operator " << name << "( ";
    if (LHS) {
      out << *LHS << ", ";
    }
    if (RHS) {
      out << *RHS;
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
};

struct FunctionDef : public FunBase {
  std::vector<std::unique_ptr<VarDeclaration>> arguments;
  FunctionDef(TokenView name, bool member = false) : FunBase(name, member) {}
  FunctionDef(TokenView name, TypeRef return_type, bool member = false,
              auto &&...args)
      : FunBase(name, return_type, member) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  std::size_t argcount() const noexcept override { return arguments.size(); }
  std::ostream &print(std::ostream &out) const override {
    out << "operator " << name << "( ";
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
};

class FunctionCall : public Expression {
public:
  const FunBase &definition;
  ExpressionPtr LHS;
  ExpressionPtr RHS;
  FunctionCall(const FunBase &Callee) : definition(Callee) {}
  FunctionCall(const FunBase &callee, ExpressionPtr &&LHS, ExpressionPtr &&RHS)
      : definition(callee), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  inline std::ostream &print(std::ostream &out) const override {
    if (LHS)
      out << *LHS << ' ';
    out << definition.getName() << ' ';
    if (RHS)
      out << *RHS;
    return out;
  }
  bool isComplete() const noexcept override {
    if (!LHS && !RHS)
      return false;
    std::size_t count;
    if (!LHS || !RHS)
      count = 1;
    else
      count = 2;
    return count == definition.argcount();
  }
  const FunBase &get_def() const noexcept { return definition; }
  TokenView getName() const noexcept override { return "op call"; }
  bool isUnaryPre() const noexcept { return !LHS && RHS; }
  bool isUnaryPost() const noexcept { return LHS && !RHS; }
  bool isBinary() const noexcept { return LHS && RHS; }
  bool isUnary() const noexcept { return !isBinary(); }
  virtual TypePtr getType() const noexcept override {
    return definition.return_type;
  }
};
} // namespace self