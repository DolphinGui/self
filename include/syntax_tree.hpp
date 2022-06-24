#pragma once

#include <cstddef>
#include <initializer_list>
#include <memory>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <polymorphic_list/list.hpp>

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
struct indirector : public expression {
  expression_ptr inner;
  indirector(expression *ptr) : inner(ptr) {}
  indirector(expression_ptr &&ptr) : inner(std::move(ptr)) {}
  ~indirector() = default;
  indirector(indirector &&other) = default;
  std::ostream &print(std::ostream &out) const override {
    out << "indirector: ";
    return inner->print(out);
  }
  bool is_complete() const override { return inner->is_complete(); }

  token_view getName() const noexcept override { return inner->getName(); };
};
struct unevaluated_expression : public expression,
                                public poly::list<expression> {
  inline std::ostream &print(std::ostream &out) const override {
    out << "Tree contents:\n";
    for (const auto &e : *this) {
      out << "  " << e << '\n';
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
};
struct namespace_tree : unevaluated_expression {
  token name;
  inline std::ostream &print(std::ostream &out) const override {
    out << "namespace " << name << " contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
  }
};

template <typename T> class literal : public expression {
  T value;

public:
  literal(T itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "literal: " << value;
  }
  inline token_view getName() const noexcept override {
    return "literal value";
  };
};

class maybe_expression : public expression {
  variant<token, expression_ptr> contents;

public:
  maybe_expression(token_view t) : contents(std::in_place_type_t<token>(), t) {}
  maybe_expression(expression_ptr &&expr)
      : contents(std::in_place_type_t<expression_ptr>(), std::move(expr)) {}
  inline auto &get_expr() { return std::get<1>(contents); }
  inline auto &get_token() { return std::get<0>(contents); }
  void confirm(expression_ptr &&ptr) { contents.emplace<1>(std::move(ptr)); }

  virtual std::ostream &print(std::ostream &stream) const {
    if (contents.index() == 0) {
      stream << "unevaluated expression: \"" << std::get<0>(contents) << "\" ";
    } else {
      std::get<1>(contents)->print(stream);
    }
    return stream;
  }
  virtual bool is_complete() const {
    if (contents.index() == 0)
      return false;
    return std::get<1>(contents)->is_complete();
  };
  virtual token_view getName() const noexcept {
    if (contents.index() == 0) {
      return "unevaluated expression";
    } else {
      return std::get<1>(contents)->getName();
    }
  };
};

// _q means qualifier
enum struct qual { const_q, ref_q, compiletime_q, runtime_q };

class var_decl : public expression {
  token name;
  vector<qual> qualifiers;

public:
  const var_decl *var_type = nullptr;
  var_decl(token_view name) : name(name) {}
  var_decl(token_view name, const var_decl &type)
      : name(name), var_type(&type) {}
  var_decl(token_view name, std::initializer_list<qual> qualifiers)
      : name(name), qualifiers(qualifiers) {}
  inline void add_qualifier(token qualifier) {}
  token_view getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    if (var_type)
      return out << "variable declaration: " << var_type->getName() << ", "
                 << name;
    else
      return out << "variable declaration: indeterminate type,  " << name;
  }
};

// name, type/value
decltype(auto) var_decl_ptr(auto &&...args) {
  return std::make_unique<var_decl>(std::forward<decltype(args)>(args)...);
}
using type_list = vector<const var_decl *>;

struct fun_def : public expression {
  token name;
  expression_list arguments;
  expression_ptr body;
  var_decl const *return_type;
  bool member = false;

  fun_def(token_view name, bool member = false) : name(name), member(member) {}
  fun_def(token_view name, const var_decl &return_type, bool member = false,
          auto &&...args)
      : name(name), return_type(&return_type), member(member) {
    arguments.reserve(sizeof...(args));
    detail::iterate([&](auto &&arg) { arguments.push_back(std::move(arg)); },
                    std::move(args)...);
  }
  inline std::ostream &print(std::ostream &out) const override {
    return out << "function: " << name;
  }
  string_view getName() const noexcept override { return name; }
  bool is_member() const noexcept { return member; }
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
};

class fun_call : public expression {
  const fun_def &function;
  expression_list args;

public:
  fun_call(const fun_def &Callee) : function(Callee) {}
  fun_call(const fun_def &callee, expression_list &&Args)
      : function(callee), args(std::move(Args)) {}
  inline std::ostream &print(std::ostream &out) const override {
    out << "function call: " << function << " args: (";
    for (const auto &arg : args) {
      out << *arg.get() << ' ';
    }
    out << ')';
    return out;
  }
  virtual token_view getName() const noexcept override {
    return "function call";
  }
  const fun_def &get_def() const noexcept { return function; }
  // TODO: make this check types later.
  bool is_complete() const noexcept override {
    return function.arguments.size() == args.size();
  }
  void add_arg(expression_ptr &&in) { args.push_back(std::move(in)); }
};

class assembly : public expression {
  token asm_string;

public:
  assembly(token_view assembly) : asm_string(assembly) {}
  inline std::ostream &print(std::ostream &out) const override {
    return out << "inline assembly";
  }
};

} // namespace selflang