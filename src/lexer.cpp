#include "lexer.hpp"
#include "container_types.hpp"
#include "fsa_parser.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <algorithm>
#include <charconv>
#include <cstddef>
#include <function_pipes.hpp>
#include <iostream>
#include <memory>
#include <ranges>
#include <re2/re2.h>
#include <stdexcept>
#include <system_error>
#include <utility>

namespace {

using namespace selflang;
void preprocess(string &contents) {
  RE2::Replace(&contents, R"(\/\/.*?\n)", " ");
  RE2::Replace(&contents, R"(\/\*[\S\s]*?\*\/)", " ");
  RE2::Replace(&contents, "\n", ";");
  contents.push_back(';');
}

auto token_parse(string &whole) {
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

auto read_file(std::istream &in) {
  in.seekg(std::ios_base::end);
  auto filesize = in.tellg();
  in.seekg(std::ios_base::beg);
  string contents;
  contents.reserve(filesize);
  in >> contents;
  return contents;
}
struct expression_v {
  statement internal;
};

} // namespace
namespace selflang {
expression_tree lex(const string &in) {
  // who knows how many copy constructors this thing calls
  // need to optimize later TODOS
  return in | mtx::pipe([](auto &&f) {
           preprocess(f);
           return f;
         }) |
         mtx::pipe([](auto &&f) { return token_parse(f); }) |
         mtx::pipe([](auto &&in) { return parse(in); });
  using namespace std::literals;
}
} // namespace selflang
  /* */