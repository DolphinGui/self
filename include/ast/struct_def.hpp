#pragma once

#include <vector>

#include "ast/expression.hpp"

namespace selflang {
struct var_decl;
struct fun_def_base;
struct struct_def : public expression {
  std::vector<var_decl> member_var;
  std::vector<fun_def_base> member_fun;
};
} // namespace selflang