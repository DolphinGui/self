#pragma once
#include "syntax_tree.hpp"
#include <cstddef>
namespace selflang {
namespace detail {
constexpr auto ctr_lambda = [](auto in, int hash) {
  in.hash = hash;
  return in;
};
enum hash_value { none = 0, store, addi, subi, muli, divi };
} // namespace detail
const inline auto type_type = var_decl("type");
const inline auto void_type = var_decl("void", type_type);
const inline auto byte_type = var_decl("byte", type_type);
const inline auto type_var = var_decl("type", type_type);
const inline auto int_type = var_decl("int", type_type);
const inline auto char_type = var_decl("char", type_type);
// const inline auto bool_type = var_decl("bool");
// might remove these later
const inline auto int_token_assignment = detail::ctr_lambda(
    operator_def(
        "=", int_type,
        var_decl_ptr("this", type_ref{.ptr = int_type, .is_ptr = true}),
        var_decl_ptr("RHS", int_type), true),
    detail::store);
const inline auto internal_addi = detail::ctr_lambda(
    operator_def("+", int_type, var_decl_ptr("LHS", int_type),
                 var_decl_ptr("RHS", int_type)),
    detail::addi);
const inline auto internal_subi = detail::ctr_lambda(
    operator_def("-", int_type, var_decl_ptr("LHS", int_type),
                 var_decl_ptr("RHS", int_type)),
    detail::subi);
const inline auto internal_muli = detail::ctr_lambda(
    operator_def("*", int_type, var_decl_ptr("LHS", int_type),
                 var_decl_ptr("RHS", int_type)),
    detail::muli);
const inline auto internal_divi = detail::ctr_lambda(
    operator_def("/", int_type, var_decl_ptr("LHS", int_type),
                 var_decl_ptr("RHS", int_type)),
    detail::divi);
using int_literal = literal<size_t>;
using char_literal = literal<unsigned char>;
using double_literal = literal<double>;

} // namespace selflang