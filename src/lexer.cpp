#include "lexer.hpp"
#include "container_types.hpp"
#include "fsa_parser.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
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

namespace {

using namespace selflang;
auto preprocess(string contents) {
  RE2::Replace(&contents, R"(\/\/.*?\n)", " ");
  RE2::Replace(&contents, R"(\/\*[\S\s]*?\*\/)", " ");
  RE2::Replace(&contents, "\n", ";");
  contents.push_back(';');
  return contents;
}

auto token_parse(string whole) {
  re2::StringPiece input(whole);
  token cur_token;
  token_vec token_list;
  while (RE2::FindAndConsume(
      &input,
      R"(((?:->)|\(|\)|{|}|\[|\]|(?:'.+')|;|,|<|>|:|[^<>\s(){};,\[\]':]+))",
      &cur_token)) {
    token_list.push_back(cur_token);
  }
  return token_list;
}


} // namespace
namespace selflang {
expression_tree lex(string in) {
  return parse(token_parse(preprocess(in)));
  using namespace std::literals;
}
} // namespace selflang
  /* */