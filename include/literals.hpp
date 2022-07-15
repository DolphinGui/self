#pragma once

#include "selfc.hpp"
#include <initializer_list>

namespace self {
using Token = std::string;
using TokenView = std::string_view;
namespace reserved {
inline constexpr TokenView struct_t = "struct";
inline constexpr TokenView var_t = "var";
inline constexpr TokenView fun_t = "fun";
inline constexpr TokenView typename_t = "typename";
inline constexpr TokenView token_t = "Token";
inline constexpr TokenView return_t = "return";
inline constexpr TokenView align_t = "align";
inline constexpr TokenView break_t = "break";
inline constexpr TokenView case_t = "case";
inline constexpr TokenView catch_t = "catch";
inline constexpr TokenView compiletime_t = "compiletime";
inline constexpr TokenView runtime_t = "runtime";
inline constexpr TokenView const_t = "const";
inline constexpr TokenView volatile_t = "volatile";
inline constexpr TokenView continue_t = "continue";
inline constexpr TokenView default_t = "default";
inline constexpr TokenView delete_t = "delete";
inline constexpr TokenView do_t = "do";
inline constexpr TokenView if_t = "if";
inline constexpr TokenView else_t = "else";
inline constexpr TokenView export_t = "export";
inline constexpr TokenView import_t = "import";
inline constexpr TokenView for_t = "for";
inline constexpr TokenView goto_t = "goto";
inline constexpr TokenView friend_t = "friend";
inline constexpr TokenView inline_t = "inline";
inline constexpr TokenView muta_t = "mut";
inline constexpr TokenView namespace_t = "namespace";
inline constexpr TokenView throw_t = "throw";
inline constexpr TokenView operator_t = "operator";
inline constexpr TokenView throws_t = "throws";
inline constexpr TokenView private_t = "private";
inline constexpr TokenView protected_t = "protected";
inline constexpr TokenView public_t = "protected";
inline constexpr TokenView sizeof_t = "sizeof";
inline constexpr TokenView static_assert_t = "static_assert";
inline constexpr TokenView static_cast_t = "static_cast";
inline constexpr TokenView dynamic_cast_t = "dynamic_cast";
inline constexpr TokenView reinterpert_cast_t = "reinterpert_cast";
inline constexpr TokenView switch_t = "switch";
inline constexpr TokenView try_t = "try";
inline constexpr TokenView typedef_t = "typedef";
inline constexpr TokenView typeid_t = "typeid";
inline constexpr TokenView void_t = "void";
inline constexpr TokenView while_t = "while";
inline constexpr TokenView byte_t = "byte";
constexpr inline auto list = {
    struct_t,      var_t,          fun_t,
    typename_t,    token_t,        return_t,
    align_t,       break_t,        case_t,
    catch_t,       compiletime_t,  runtime_t,
    const_t,       volatile_t,     continue_t,
    default_t,     delete_t,       do_t,
    if_t,          else_t,         export_t,
    import_t,      for_t,          goto_t,
    friend_t,      inline_t,       muta_t,
    namespace_t,   throw_t,        operator_t,
    throws_t,      private_t,      protected_t,
    public_t,      sizeof_t,       static_assert_t,
    static_cast_t, dynamic_cast_t, reinterpert_cast_t,
    switch_t,      try_t,          typedef_t,
    typeid_t,      void_t,         while_t,
    byte_t,
};
constexpr inline auto qualifiers = {
    align_t,  compiletime_t, runtime_t, const_t,  volatile_t, export_t,
    import_t, for_t,         friend_t,  inline_t, muta_t};
bool inline constexpr isKeyword(TokenView t) {
  for (auto a : list) {
    if (a == t)
      return true;
  }
  return false;
}
inline constexpr TokenView endl = ";";
constexpr inline std::initializer_list<const char *> grammar = {
    ";", ",", " ", "(", ")", "{", "}", "\"", ":"};
bool inline isGrammar(TokenView t) {
  for (auto g : grammar) {
    if (t == g)
      return true;
  }
  return false;
}
bool inline constexpr is_reserved(TokenView t) {
  return isKeyword(t) || isGrammar(t);
}

bool inline constexpr is_qualifier(TokenView t) {
  for (auto a : qualifiers) {
    if (a == t)
      return true;
  }
  return false;
}
} // namespace reserved
} // namespace self