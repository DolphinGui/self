#pragma once

#include <cstddef>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#include <container_types.hpp>

namespace cplang {
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
};

struct type : public expression {
  virtual ~type() = default;
  virtual string_view getName() const noexcept = 0;
};

struct symbol : public expression {
  virtual ~symbol() = default;
  virtual string_view getName() const noexcept = 0;
};

using name_list = vector<std::unique_ptr<type>>;
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

using int_literal = literal<size_t>;
using float_literal = literal<double>;

class byte_type_t : public type {
public:
  byte_type_t() = default;
  inline std::ostream &print(std::ostream &out) const override {
    return out << "byte type";
  }
  string_view getName() const noexcept override { return "byte"; }
};
constexpr inline byte_type_t byte_type;

class struct_type_t : public type {
public:
  struct_type_t() = default;
  inline std::ostream &print(std::ostream &out) const override {
    return out << "struct type";
  }
  string_view getName() const noexcept override { return "struct data"; }
};
constexpr inline struct_type_t struct_type_t;

class void_type_t : public type {
public:
  void_type_t() = default;
  inline std::ostream &print(std::ostream &out) const override {
    return out << "void type";
  }
  string_view getName() const noexcept override { return "void"; }
};
constexpr inline void_type_t void_type;

class var_decl : public symbol {
  token name;
  const type &var_type;

public:
  var_decl(token_view name, const type &type) : name(name), var_type(type) {}
  token_view getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable declaration: " << var_type.getName() << " " << name;
  }
};

struct funct_def : public symbol {
  token name;
  expression_list args;
  expression_ptr body;

  funct_def(token_view name, expression_list &&args, expression_ptr &&body)
      : name(name), args(std::move(args)), body(std::move(body)) {}
  inline std::ostream &print(std::ostream &out) const override {
    return out << "function declaration: " << name;
  }
  string_view getName() const noexcept override { return name; }
};

class struct_decl : public type {
  token name;
  expression_list body;

public:
  struct_decl(token_view name) : name(name) {}
  struct_decl(token_view name, expression_list &&body)
      : name(name), body(std::move(body)) {}
  inline void define(expression_list &&body_list) {
    if (!body.empty())
      throw std::runtime_error("ODR defied");
    body = std::move(body_list);
  }
  inline bool is_complete() const override { return !body.empty(); }
  inline std::ostream &print(std::ostream &out) const override {
    return out << "struct declaration: " << name;
  }

  string_view getName() const noexcept override { return name; }
};

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
  funct_call(funct_def &Callee, expression_list &&Args)
      : function(Callee), args(std::move(Args)) {}
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

} // namespace cplang