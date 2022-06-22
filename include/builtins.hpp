#pragma once
#include "syntax_tree.hpp"
#include <cstddef>
namespace selflang {

const inline auto void_type = var_decl("void");
const inline auto byte_type = var_decl("byte");
const inline auto type_var = var_decl("type");
const inline auto int_type = var_decl("int");
// might remove these later
const inline auto int_token_t = var_decl("int_token", type_var);
const inline auto int_token_assignment =
    operator_def("=", int_type, var_decl_ptr("this", int_token_t),
                 var_decl_ptr("RHS", int_token_t), true);
const inline auto internal_addi =
    operator_def("+", int_type, var_decl_ptr("LHS", int_token_t),
                 var_decl_ptr("RHS", int_token_t));
const inline auto internal_subi =
    operator_def("-", int_type, var_decl_ptr("LHS", int_token_t),
                 var_decl_ptr("RHS", int_token_t));
const inline auto internal_muli =
    operator_def("*", int_type, var_decl_ptr("LHS", int_token_t),
                 var_decl_ptr("RHS", int_token_t));
const inline auto internal_divi =
    operator_def("/", int_type, var_decl_ptr("LHS", int_token_t),
                 var_decl_ptr("RHS", int_token_t));
using int_literal = literal<size_t>;
using double_literal = literal<double>;

} // namespace selflang