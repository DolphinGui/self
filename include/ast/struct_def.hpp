#pragma once

#include <memory>
#include <vector>

#include "ast/expression.hpp"

namespace selflang {
class var_decl;
struct fun_def_base;
struct struct_def : public expression {
  std::vector<std::unique_ptr<var_decl>> member_var;
  std::vector<std::unique_ptr<fun_def_base>> member_fun;
};
} // namespace selflang