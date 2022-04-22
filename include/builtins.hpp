#pragma once
#include "syntax_tree.hpp"
namespace selflang {

const inline auto void_type = var_decl("void");
const inline auto byte_type = var_decl("byte");
const inline auto typename_type = var_decl("typename");
const inline auto int_token_t = var_decl("int_token", typename_type);
const inline auto int_token_assignment = operator_def(
    "=", var_decl_ptr("this", int_token_t), var_decl_ptr("RHS", int_token_t));
const inline auto internal_addi = operator_def(
    "+", var_decl_ptr("LHS", int_token_t), var_decl_ptr("RHS", int_token_t));
} // namespace selflang