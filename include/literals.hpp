#pragma once

#include "cp.hpp"

namespace cplang {
using token = std::string;
using token_view = std::string_view;
namespace reserved_tokens {
inline constexpr token_view struct_t = "struct";
inline constexpr token_view typename_t = "typename";
inline constexpr token_view token_t = "token";
inline constexpr token_view return_t = "return";
inline constexpr token_view align_t = "align";
inline constexpr token_view break_t = "break";
inline constexpr token_view case_t = "case";
inline constexpr token_view catch_t = "catch";
inline constexpr token_view compiletime_t = "compiletime";
inline constexpr token_view runtime_t = "runtime";
inline constexpr token_view const_t = "const";
inline constexpr token_view volatile_t = "volatile";
inline constexpr token_view continue_t = "continue";
inline constexpr token_view default_t = "default";
inline constexpr token_view delete_t = "delete";
inline constexpr token_view do_t = "do";
inline constexpr token_view if_t = "if";
inline constexpr token_view else_t = "else";
inline constexpr token_view export_t = "export";
inline constexpr token_view import_t = "import";
inline constexpr token_view for_t = "for";
inline constexpr token_view goto_t = "goto";
inline constexpr token_view friend_t = "friend";
inline constexpr token_view inline_t = "inline";
inline constexpr token_view muta_t = "mut";
inline constexpr token_view namespace_t = "namespace";
inline constexpr token_view throw_t = "throw";
inline constexpr token_view operator_t = "operator";
inline constexpr token_view throws_t = "throws";
inline constexpr token_view private_t = "private";
inline constexpr token_view protected_t = "protected";
inline constexpr token_view public_t = "protected";
inline constexpr token_view sizeof_t = "sizeof";
inline constexpr token_view static_assert_t = "static_assert";
inline constexpr token_view static_cast_t = "static_cast";
inline constexpr token_view dynamic_cast_t = "dynamic_cast";
inline constexpr token_view reinterpert_cast_t = "reinterpert_cast";
inline constexpr token_view switch_t = "switch";
inline constexpr token_view try_t = "try";
inline constexpr token_view typedef_t = "typedef";
inline constexpr token_view typeid_t = "typeid";
inline constexpr token_view void_t = "void";
inline constexpr token_view while_t = "while";
inline constexpr token_view byte_t = "byte";
bool inline constexpr is_reserved(token_view t) {
  for (auto a : {
           struct_t,
           typename_t,
           token_t,
           return_t,
           align_t,
           break_t,
           case_t,
           catch_t,
           compiletime_t,
           runtime_t,
           const_t,
           volatile_t,
           continue_t,
           default_t,
           delete_t,
           do_t,
           if_t,
           else_t,
           export_t,
           import_t,
           for_t,
           goto_t,
           friend_t,
           inline_t,
           muta_t,
           namespace_t,
           throw_t,
           operator_t,
           throws_t,
           private_t,
           protected_t,
           public_t,
           sizeof_t,
           static_assert_t,
           static_cast_t,
           dynamic_cast_t,
           reinterpert_cast_t,
           switch_t,
           try_t,
           typedef_t,
           typeid_t,
           void_t,
           while_t,
           byte_t,
       }) {
    if (a == t)
      return true;
  }
  return false;
}
} // namespace reserved_tokens
} // namespace cplang