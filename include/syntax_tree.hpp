#pragma once

#include <memory>
#include <string>
#include <vector>

namespace cplang {
using token = std::string;
using token_view = std::string_view;
using name_list = std::vector<token>;
// there are declaration statements, and evaluative statements.
struct expression {
  virtual ~expression() = 0;
};
using expression_ptr = std::unique_ptr<expression>;
using expression_list = std::vector<expression_ptr>;

class int_literal : public expression {
  int value;

public:
  int_literal(token_view itself) : value(std::stoi(itself.data())){};
};

class var_ref : public expression {
  token name;

public:
  var_ref(token_view itself) : name(itself){};
};

class funct_call : public expression {
  token function;
  expression_list args;

public:
  funct_call(token_view Callee, expression_list Args)
      : function(Callee), args(std::move(Args)) {}
};

class var_decl : public expression{
  token name, type;

public:
  var_decl(token_view name, token_view type) : name(name), type(type) {}
  token_view getName() const noexcept { return name; }
};

class funct_def : public expression{
  token name;
  expression_list args;
  expression_ptr body;

public:
  funct_def(token_view name, expression_list args, expression_ptr body)
      : name(name), args(args), body(std::move(body)) {}
};
class assembly : public expression{
  token asm_string;
  public:
  assembly(token_view assembly): asm_string(assembly){}
};
} // namespace cplang