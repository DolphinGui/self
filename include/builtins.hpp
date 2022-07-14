#pragma once
#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"
#include <cstddef>
#include <string_view>
namespace selflang {
namespace detail {
constexpr auto ctr_lambda = [](auto in, int hash) {
  in.hash = hash;
  return in;
};
enum hash_value { none = 0, store, addi, subi, muli, divi };
} // namespace detail
const inline auto type_type = [] {
  auto type = builtin_type("type");
  // the type of type is type.
  type.register_type({type}, typeid(struct_def));
  type.register_type({type}, typeid(opaque_struct));
  return type;
}();
const inline auto void_type = builtin_type("void", {type_type}, typeid(void));
const inline auto byte_type =
    builtin_type("byte", {type_type}, typeid(std::byte));
const inline auto int_type = builtin_type("i32", {type_type}, typeid(size_t));
const inline auto char_type =
    builtin_type("char", {type_type}, typeid(unsigned char));
const inline auto int_any = type_ref{.ptr = int_type, .is_ref = ref_types::any};
const inline auto i32_assignment = detail::ctr_lambda(
    operator_def("=", int_type,
                 var_decl_ptr("this", type_ref{.ptr = int_type,
                                               .is_ref = ref_types::ref}),
                 var_decl_ptr("RHS", int_any), true),
    detail::store);
const inline auto addi =
    detail::ctr_lambda(operator_def("+", int_type, var_decl_ptr("LHS", int_any),
                                    var_decl_ptr("RHS", int_any)),
                       detail::addi);
const inline auto subi =
    detail::ctr_lambda(operator_def("-", int_type, var_decl_ptr("LHS", int_any),
                                    var_decl_ptr("RHS", int_any)),
                       detail::subi);
const inline auto muli =
    detail::ctr_lambda(operator_def("*", int_type, var_decl_ptr("LHS", int_any),
                                    var_decl_ptr("RHS", int_any)),
                       detail::muli);
const inline auto divi =
    detail::ctr_lambda(operator_def("/", int_type, var_decl_ptr("LHS", int_any),
                                    var_decl_ptr("RHS", int_any)),
                       detail::divi);
const inline auto struct_assignment = operator_def(
    "=", type_ref{.ptr = type_type, .is_ref = ref_types::ref},
    var_decl_ptr("this", type_ref{.ptr = type_type, .is_ref = ref_types::ref}),
    var_decl_ptr("RHS", type_ref{.ptr = type_type, .is_ref = ref_types::value}),
    true);

using int_literal = literal<size_t>;
using char_literal = literal<unsigned char>;
using double_literal = literal<double>;
using struct_literal = literal<struct_def>;
using opaque_struct_literal = literal<opaque_struct>;
using token_view = std::string_view;
template <> struct literal<std::vector<unsigned char>> : public expression {
  std::vector<unsigned char> value;
  literal(std::vector<unsigned char> itself) : value(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    out << "literal: \"";
    for (char c : value) {
      out << c;
    }
    return out << '\"';
  }
  inline token_view getName() const noexcept override {
    return "literal value";
  };
};

using string_literal = literal<std::vector<unsigned char>>;

} // namespace selflang