#include <algorithm>
#include <charconv>
#include <cstddef>
#include <iostream>
#include <memory>
#include <ranges>
#include <re2/re2.h>
#include <stdexcept>
#include <system_error>
#include <utility>

#include "ast/expression_tree.hpp"
#include "fsa_parser.hpp"
#include "lexer.hpp"
#include "literals.hpp"

namespace self {
namespace detail {
std::string preprocess(std::string contents) {
  RE2::GlobalReplace(&contents, R"(\/\/.*?\n)", " ");
  RE2::GlobalReplace(&contents, R"(\/\*[\S\s]*?\*\/)", " ");
  RE2::GlobalReplace(&contents, "\n", ";");
  contents.push_back(';');
  return contents;
}

self::TokenVec parseToken(std::string whole) {
  re2::StringPiece input(whole);
  Token cur_Token;
  TokenVec Token_list;
  while (RE2::FindAndConsume(
      &input, R"(((?:->|'.+?'|".+?")|[(){}[\];,<>:]|[^<>\s(){};,[\]'":]+))",
      &cur_Token)) {
    Token_list.push_back(cur_Token);
  }
  return Token_list;
}
} // namespace detail
Module lex(std::string in, Context &c) {
  return parse(detail::parseToken(detail::preprocess(in)), c);
}
} // namespace self