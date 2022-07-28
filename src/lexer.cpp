#include <algorithm>
#include <charconv>
#include <cstddef>
#include <iostream>
#include <memory>
#include <ranges>
#include <re2/re2.h>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "ast/expression_tree.hpp"
#include "fsa_parser.hpp"
#include "lexer.hpp"
#include "literals.hpp"

namespace {} // namespace

namespace self {
namespace detail {
std::string &preprocess(std::string &contents) {
  RE2::GlobalReplace(&contents, R"(\/\/.*?\n)", " ");
  RE2::GlobalReplace(&contents, R"(\/\*[\S\s]*?\*\/)", " ");
  // funny thing is regex doesn't capture newline all that
  // well so it's replaced with a character I assume won't
  // turn up in normal files
  RE2::GlobalReplace(&contents, "\n", "\03");
  contents.push_back(';');
  return contents;
}

self::LexedFileRef parseToken(std::string &whole) {
  auto input = re2::StringPiece(whole);
  re2::StringPiece cur_token;
  TokenVec token_list;
  std::vector<unsigned> line_pos;
  line_pos.push_back(0);
  while (RE2::FindAndConsume(
      &input, R"(((?:->|'.+?'|".+?")|[(){}[\];,:.\03]|[^\s(){};,[\]'":.\03]+))",
      &cur_token)) {
    token_list.push_back(std::string_view(cur_token.data(), cur_token.size()));
    if (cur_token == "\n") {
      auto pos = std::distance(static_cast<const char *>(whole.data()),
                               cur_token.data());
      line_pos.push_back(pos);
    }
  }
  line_pos.erase(line_pos.begin());
  return {std::move(token_list), whole, std::move(line_pos)};
}
} // namespace detail
Module parseFile(std::string &in, Context &c) {
  auto n = detail::parseToken(detail::preprocess(in));
  return parse(n, c);
}
} // namespace self