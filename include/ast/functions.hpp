#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/variables.hpp"

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
  std::vector<std::unique_ptr<var_decl>> arguments;
  expression_tree body;
  var_decl const *return_type = nullptr;
  int hash = 0;
  bool member = false;
  bool body_defined = false;

protected:
  fun_def_base(token_view name, bool member = false)
      : name(name), member(member) {}
  fun_def_base(token_view name, const var_decl &return_type,
               bool member = false, auto &&...args)
      : name(name), return_type(&return_type), member(member) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }

public:
  std::ostream &print(std::ostream &out) const override;
  std::string_view getName() const noexcept override { return name; }
  bool is_member() const noexcept { return member; }
  bool is_complete() const noexcept override { return return_type; }
};
decltype(auto) funct_decl_ptr(auto &&...args) {
  return std::make_unique<fun_def_base>(std::forward<decltype(args)>(args)...);
}

class operator_def : public fun_def_base {
public:
  operator_def(token_view name, const var_decl &return_type,
               std::unique_ptr<var_decl> &&LHS, std::unique_ptr<var_decl> &&RHS,
               bool member = false)
      : fun_def_base(name, return_type, member, std::move(LHS),
                     std::move(RHS)) {}
};
struct fun_def : public fun_def_base {
  fun_def(token_view name, bool member = false) : fun_def_base(name, member) {}
  fun_def(token_view name, const var_decl &return_type, bool member = false,
          auto &&...args)
      : fun_def_base(name, return_type, member, args...) {}
};
decltype(auto) operator_decl_ptr(auto &&...args) {
  return std::make_unique<operator_def>(std::forward<decltype(args)>(args)...);
}

struct fun_call_base : public expression {
  const fun_def_base &definition;
  expression_list args;

  fun_call_base(const fun_def_base &Callee) : definition(Callee) {}
  fun_call_base(const fun_def_base &callee, expression_list &&Args)
      : definition(callee), args(std::move(Args)) {}
  inline std::ostream &print(std::ostream &out) const override {
    out << "function call: " << definition << " args: (";
    for (const auto &arg : args) {
      out << *arg.get() << ' ';
    }
    out << ')';
    return out;
  }
  const fun_def_base &get_def() const noexcept { return definition; }
  // TODO: make this check types later.
  bool is_complete() const noexcept override {
    return definition.arguments.size() == args.size();
  }
  void add_arg(expression_ptr &&in) { args.push_back(std::move(in)); }
};

class op_call : public fun_call_base {
public:
  op_call(const fun_def_base &Callee) : fun_call_base(Callee) {}
  op_call(const fun_def_base &callee, expression_list &&Args)
      : fun_call_base(callee, std::forward<expression_list>(Args)) {}

  virtual token_view getName() const noexcept override { return "op call"; }
};
class fun_call : public fun_call_base {
public:
  fun_call(const fun_def_base &Callee) : fun_call_base(Callee) {}
  fun_call(const fun_def_base &callee, expression_list &&Args)
      : fun_call_base(callee, std::forward<expression_list>(Args)) {}

  virtual token_view getName() const noexcept override { return "fun call"; }
};
} // namespace selflang