#pragma once
#include <memory>
#include <sstream>

#include "container_types.hpp"
namespace selflang {
namespace detail {
inline void iterate(auto lambda, auto &&arg) { lambda(arg); }
inline void iterate(auto lambda, auto &&arg, auto &&...args) {
  lambda(arg);
  iterate(lambda, args...);
}
} // namespace detail
using token = string;
using token_view = string_view;
// there are declaration statements, and evaluative statements.
// I think pointers to pointers are stupid
// and maybe I'll figure out something
// that fragments memory less
// until then this'll just be slow
// would love to do this with variant types
struct expression {
  virtual ~expression() = default;
  virtual std::ostream &print(std::ostream &) const = 0;
  virtual bool is_complete() const { return true; };
  friend std::ostream &operator<<(std::ostream &os, expression const &ex) {
    return ex.print(os);
  }
  virtual token_view getName() const noexcept = 0;
};

using expression_ptr = std::unique_ptr<expression>;
using expression_list = vector<expression_ptr>;

// This is now a vector of pointers. This is horrible, but figuring
// a better structure out is difficult.
struct expression_tree : public expression, public expression_list {
  inline std::ostream &print(std::ostream &out) const override {
    out << "Tree contents:\n";
    for (const auto &e : *this) {
      out << "  " << *e << '\n';
    }
    return out;
  }
  std::string dump_contents() {
    std::stringstream result;
    print(result);
    return result.str();
  }
  virtual token_view getName() const noexcept override {
    return "expression tree";
  }
  bool is_complete() const noexcept override {
    for (auto &expr : *this) {
      if (!expr->is_complete())
        return false;
    }
    return true;
  }
};
struct namespace_tree : expression_tree {
  token name;
  inline std::ostream &print(std::ostream &out) const override {
    out << "namespace " << name << " contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
  }
};

template <typename T> struct literal : public expression {
  T value;
  literal(T itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "literal: " << value;
  }
  inline token_view getName() const noexcept override {
    return "literal value";
  };
};

class maybe_expression : public expression {
  token contents;

public:
  maybe_expression(token_view t) : contents(t) {}
  inline token_view get_token() { return contents; }
  virtual std::ostream &print(std::ostream &stream) const override {
    return stream << "unevaluated expression: \"" << contents << "\" ";
  }
  virtual bool is_complete() const override { return false; }
  virtual token_view getName() const noexcept override {
    return "unevaluated expression";
  }
};
class var_decl;
template <typename T> class type_indirect {
public:
  T ptr;
  // These are qualifiers.
  bool is_ptr = false;
};
using type_ptr = type_indirect<const var_decl *>;
using type_ref = type_indirect<const var_decl &>;

class var_decl : public expression {
  token name;

public:
  type_ptr type;
  var_decl(token_view name) : name(name), type{nullptr} {}
  var_decl(token_view name, type_ref type) : name(name), type{&type.ptr} {}
  var_decl(token_view name, const var_decl &type) : name(name), type{&type} {}

  token_view getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    if (type.ptr)
      return out << "variable declaration: " << type.ptr->getName() << ", "
                 << name;
    else
      return out << "variable declaration: indeterminate type,  " << name;
  }
};

// name, type/value
std::unique_ptr<var_decl> var_decl_ptr(auto &&...args) {
  return std::make_unique<var_decl>(std::forward<decltype(args)>(args)...);
}
using type_list = vector<const var_decl *>;

struct fun_def : public expression {
  token name;
  vector<std::unique_ptr<var_decl>> arguments;
  expression_tree body;
  var_decl const *return_type = nullptr;
  int hash = 0;
  bool member = false;
  bool body_defined = false;

  fun_def(token_view name, bool member = false) : name(name), member(member) {}
  fun_def(token_view name, const var_decl &return_type, bool member = false,
          auto &&...args)
      : name(name), return_type(&return_type), member(member) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  inline std::ostream &print(std::ostream &out) const override {
    out << "function: " << name;
    if (return_type) {
      out << " returns " << *return_type;
    }

    if (!arguments.empty()) {
      out << "\nargs: ";
      for (auto &arg : arguments) {
        out << *arg << '\n';
      }
    }
    if (!body.empty()) {
      out << "\nbody:\n";
      for (auto &p : body) {
        out << *p.get();
      }
    }
    return out;
  }
  string_view getName() const noexcept override { return name; }
  bool is_member() const noexcept { return member; }
  bool is_complete() const noexcept override { return return_type; }
};
decltype(auto) funct_decl_ptr(auto &&...args) {
  return std::make_unique<fun_def>(std::forward<decltype(args)>(args)...);
}

class operator_def : public fun_def {
public:
  operator_def(token_view name, const var_decl &return_type,
               std::unique_ptr<var_decl> &&LHS, std::unique_ptr<var_decl> &&RHS,
               bool member = false)
      : fun_def(name, return_type, member, std::move(LHS), std::move(RHS)) {}
};
decltype(auto) operator_decl_ptr(auto &&...args) {
  return std::make_unique<operator_def>(std::forward<decltype(args)>(args)...);
}
class struct_def : public expression {
  expression_list body;

public:
  struct_def(expression_list &&body) : body(std::move(body)) {}
  struct_def(auto &&...body) : body({std::move(body)...}) {}
  inline void define(expression_list &&body_list) {
    if (!body.empty())
      throw std::runtime_error("ODR defied");
    body = std::move(body_list);
  }
  inline std::ostream &print(std::ostream &out) const override {
    return out << "struct definition";
  }
  inline token_view getName() const noexcept override { return "struct def"; }
};
decltype(auto) struct_def_ptr(auto &&...args) {
  return std::make_unique<struct_def>(std::forward(args)...);
}

class var_ref : public expression {
  const var_decl &name;

public:
  var_ref(var_decl &itself) : name(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable dereference: " << name;
  }
  inline string_view getName() const noexcept override { return "var ref"; }
};

struct fun_call_base : public expression {
  const fun_def &definition;
  expression_list args;

  fun_call_base(const fun_def &Callee) : definition(Callee) {}
  fun_call_base(const fun_def &callee, expression_list &&Args)
      : definition(callee), args(std::move(Args)) {}
  inline std::ostream &print(std::ostream &out) const override {
    out << "function call: " << definition << " args: (";
    for (const auto &arg : args) {
      out << *arg.get() << ' ';
    }
    out << ')';
    return out;
  }
  const fun_def &get_def() const noexcept { return definition; }
  // TODO: make this check types later.
  bool is_complete() const noexcept override {
    return definition.arguments.size() == args.size();
  }
  void add_arg(expression_ptr &&in) { args.push_back(std::move(in)); }
};

class op_call : public fun_call_base {
public:
  op_call(const fun_def &Callee) : fun_call_base(Callee) {}
  op_call(const fun_def &callee, expression_list &&Args)
      : fun_call_base(callee, std::forward<expression_list>(Args)) {}

  virtual token_view getName() const noexcept override { return "op call"; }
};
class fun_call : public fun_call_base {
public:
  fun_call(const fun_def &Callee) : fun_call_base(Callee) {}
  fun_call(const fun_def &callee, expression_list &&Args)
      : fun_call_base(callee, std::forward<expression_list>(Args)) {}

  virtual token_view getName() const noexcept override { return "fun call"; }
};
struct ret : public expression {
  expression_ptr value;

  virtual token_view getName() const noexcept override { return "return"; }
  inline std::ostream &print(std::ostream &out) const override {
    out << "returns " << *value;
    return out;
  }
  bool is_complete() const noexcept override { return !value; }
};
} // namespace selflang