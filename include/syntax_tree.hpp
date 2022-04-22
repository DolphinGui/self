#pragma once

#include <cstddef>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <container_types.hpp>

namespace selflang {
using token = string;
using token_view = string_view;
// there are declaration statements, and evaluative statements.
// I think pointers to pointers are stupid
// and maybe I'll figure out something
// that fragments memory less
// until then this'll just be slow
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

struct expression_tree : public expression, public expression_list {
  inline std::ostream &print(std::ostream &out) const override {
    out << "Tree contents:\n";
    for (const auto &e : *this) {
      out << e << '\n';
    }
    return out;
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

template <typename T> class literal : public expression {
  T value;

public:
  literal(T itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "literal: " << value;
  }
};

class var_decl : public expression {
  token name;

public:
  const var_decl *var_type = nullptr;
  expression_ptr value;
  var_decl(token_view name) : name(name) {}
  var_decl(token_view name, const var_decl &type)
      : name(name), var_type(&type) {}
  var_decl(token_view name, expression_ptr &&value)
      : name(name), value(std::move(value)) {}
  var_decl(token_view name, const var_decl &type, expression_ptr &&value)
      : name(name), var_type(&type), value(std::move(value)) {}
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

struct funct_def : public expression {
  token name;
  expression_list args;
  expression_ptr body;
  bool member = false;

  funct_def(token_view name, expression_list &&args, expression_ptr &&body,
            bool member = false)
      : name(name), args(std::move(args)), body(std::move(body)),
        member(member) {}
  funct_def(token_view name, auto &&...args)
      : name(name), args({std::move(args)...}) {}
  inline std::ostream &print(std::ostream &out) const override {
    return out << "function declaration: " << name;
  }
  string_view getName() const noexcept override { return name; }
};
decltype(auto) funct_decl_ptr(auto &&...args) {
  return std::make_unique<funct_def>(std::forward<decltype(args)>(args)...);
}

class operator_def : public funct_def {
public:
  operator_def(token_view name, std::unique_ptr<var_decl> &&LHS,
               std::unique_ptr<var_decl> &&RHS, bool member = false)
      : funct_def(name, std::move(LHS), std::move(RHS)) {
    member = member;
  }
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

class funct_call : public expression {
  const funct_def &function;
  expression_list args;

public:
  funct_call(funct_def &Callee) : function(Callee) {}
  funct_call(funct_def &callee, expression_list &&Args)
      : function(callee), args(std::move(Args)) {}
  inline std::ostream &print(std::ostream &out) const override {
    return out << "function call: " << function;
  }
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