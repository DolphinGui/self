#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"
#include <cstddef>
#include <memory>

namespace selflang {

namespace detail {
inline void iterate(auto lambda, auto &&arg) { lambda(arg); }
inline void iterate(auto lambda, auto &&arg, auto &&...args) {
  lambda(arg);
  iterate(lambda, args...);
}
} // namespace detail

struct fun_def_base : public expression {
  token name;
  expression_tree body;
  type_ptr return_type = {nullptr};
  int hash = 0;
  bool member = false;
  bool body_defined = false;

protected:
  fun_def_base(token_view name, bool member = false)
      : name(name), member(member) {}
  fun_def_base(token_view name, const var_decl &return_type,
               bool member = false)
      : name(name), return_type{&return_type}, member(member) {}

  fun_def_base(token_view name, type_ref return_type, bool member = false)
      : name(name), return_type{.ptr = &return_type.ptr,
                                .is_ref = return_type.is_ref},
        member(member) {}

public:
  // virtual std::ostream &print(std::ostream &out) const override;
  std::string_view getName() const noexcept override { return name; }
  bool is_member() const noexcept { return member; }
  bool isComplete() const noexcept override { return return_type.ptr; }
  virtual std::size_t argcount() const noexcept = 0;
};
decltype(auto) funct_decl_ptr(auto &&...args) {
  return std::make_unique<fun_def_base>(std::forward<decltype(args)>(args)...);
}

class operator_def : public fun_def_base {
public:
  std::unique_ptr<var_decl> LHS;
  std::unique_ptr<var_decl> RHS;
  operator_def(token_view name, const var_decl &return_type,
               std::unique_ptr<var_decl> &&LHS, std::unique_ptr<var_decl> &&RHS,
               bool member = false)
      : fun_def_base(name, return_type, member), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  operator_def(token_view name, type_ref return_type,
               std::unique_ptr<var_decl> &&LHS, std::unique_ptr<var_decl> &&RHS,
               bool member = false)
      : fun_def_base(name, return_type, member), LHS(std::move(LHS)),
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
      out << " -> " << return_type.ptr->getName();
    }
    if (body_defined) {
      out << '{' << body << '}';
    }
    return out;
  }
};

struct fun_def : public fun_def_base {
  std::vector<std::unique_ptr<var_decl>> arguments;
  fun_def(token_view name, bool member = false) : fun_def_base(name, member) {}
  fun_def(token_view name, const var_decl &return_type, bool member = false,
          auto &&...args)
      : fun_def_base(name, return_type, member) {
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
      out << " -> " << return_type.ptr->getName();
    }
    if (body_defined) {
      out << '{' << body << '}';
    }
    return out;
  }
};
decltype(auto) operator_decl_ptr(auto &&...args) {
  return std::make_unique<operator_def>(std::forward<decltype(args)>(args)...);
}

class op_call : public expression {
public:
  const fun_def_base &definition;
  expression_ptr LHS;
  expression_ptr RHS;
  op_call(const fun_def_base &Callee) : definition(Callee) {}
  op_call(const fun_def_base &callee, expression_ptr &&LHS,
          expression_ptr &&RHS)
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
  const fun_def_base &get_def() const noexcept { return definition; }
  token_view getName() const noexcept override { return "op call"; }
  bool isUnaryPre() const noexcept { return !LHS && RHS; }
  bool isUnaryPost() const noexcept { return LHS && !RHS; }
  bool isBinary() const noexcept { return LHS && RHS; }
  bool isUnary() const noexcept { return !isBinary(); }
  virtual type_ptr getType() const noexcept override {
    return definition.return_type;
  }
};
// params is either a value or a tuple of values
inline decltype(auto) fun_call(const fun_def_base &callee,
                               expression_ptr &&params) {
  return std::make_unique<op_call>(callee, nullptr, std::move(params));
}
} // namespace selflang