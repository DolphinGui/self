#pragma once

#include <memory>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace cplang {
using token = std::string;
using token_view = std::string_view;
// there are declaration statements, and evaluative statements.
// I think pointers to pointers are stupid
// and maybe I'll figure out something
// that fragments memory less
// until then this'll just be slow
struct expression {
  virtual ~expression() = default;
  virtual std::ostream &print(std::ostream &) const = 0;
  friend std::ostream &operator<<(std::ostream &os, expression const &ex) {
    return ex.print(os);
  }
};

struct type {
  virtual ~type() = default;
  virtual std::string_view getName() const noexcept = 0;
};

using name_list = std::vector<std::unique_ptr<type>>;
using expression_ptr = std::unique_ptr<expression>;
using expression_list = std::vector<expression_ptr>;

class int_literal : public expression {
  int value;

public:
  int_literal(token_view itself) : value(std::stoi(itself.data())){};
  inline std::ostream &print(std::ostream &out) const {
    return out << "int literal: " << value;
  }
};

class byte_type_t : public type {
public:
  byte_type_t() = default;
  inline std::ostream &print(std::ostream &out) const {
    return out << "byte type";
  }
  std::string_view getName() const noexcept override { return "byte"; }
};
constexpr inline byte_type_t byte_type;

class void_type_t : public type {
public:
  void_type_t() = default;
  inline std::ostream &print(std::ostream &out) const {
    return out << "byte type";
  }
  std::string_view getName() const noexcept override { return "void"; }
};
constexpr inline void_type_t void_type;

class var_ref : public expression {
  token name;

public:
  var_ref(token_view itself) : name(itself){};
  inline std::ostream &print(std::ostream &out) const {
    return out << "variable dereference: " << name;
  }
};

class funct_call : public expression {
  token function;
  expression_list args;

public:
  funct_call(token_view Callee, expression_list &&Args)
      : function(Callee), args(std::move(Args)) {}
  inline std::ostream &print(std::ostream &out) const {
    return out << "function call: " << function;
  }
};

class var_decl : public expression {
  token name;
  const type &var_type;

public:
  var_decl(token_view name, const type &type) : name(name), var_type(type) {}
  token_view getName() const noexcept { return name; }
  inline std::ostream &print(std::ostream &out) const {
    return out << "variable declaration: " << var_type.getName() << " " << name;
  }
};

class funct_def : public expression {
  token name;
  expression_list args;
  expression_ptr body;

public:
  funct_def(token_view name, expression_list &&args, expression_ptr &&body)
      : name(name), args(std::move(args)), body(std::move(body)) {}
  inline std::ostream &print(std::ostream &out) const {
    return out << "function declaration: " << name;
  }
};

class struct_decl : public expression, public type {
  token name;

public:
  struct_decl(token_view name) : name(name) {}
  inline std::ostream &print(std::ostream &out) const {
    return out << "struct declaration: " << name;
  }

  std::string_view getName() const noexcept { return name; }
};

class assembly : public expression {
  token asm_string;

public:
  assembly(token_view assembly) : asm_string(assembly) {}
  inline std::ostream &print(std::ostream &out) const {
    return out << "inline assembly";
  }
};
} // namespace cplang