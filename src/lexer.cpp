#include "lexer.hpp"
#include "container_types.hpp"
#include "fsa_parser.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <algorithm>
#include <boost/sml.hpp>
#include <charconv>
#include <cstddef>
#include <function_pipes.hpp>
#include <iostream>
#include <memory>
#include <ranges>
#include <re2/re2.h>
#include <rva/variant.hpp>
#include <stdexcept>
#include <system_error>
#include <utility>

namespace {

template <typename T> using sm = boost::sml::sm<T>;
using namespace cplang;
void preprocess(string &contents) {
  RE2::Replace(&contents, "\\\n", " ");
  RE2::Replace(&contents, "\\/\\/.+?\\n", " ");
  RE2::Replace(&contents, "\\/\\*[\\S\\s]+?\\*\\/", " ");
  RE2::Replace(&contents, ";", "\n");
  contents.push_back('\n');
}

auto token_parse(string &whole) {
  re2::StringPiece input(whole);
  token cur_token;
  token_vec token_list;
  while (RE2::FindAndConsume(
      &input,
      "([^<>\\s(){},\\[\\]\"]+|\\(|\\)|{|}|\\[|\\]|(?:\".+\")|\\n|,|<|>)",
      &cur_token)) {
    token_list.push_back(cur_token);
  }
  return token_list;
}

auto statement_parse(token_vec &&tokens) {
  token_vec token_list = std::move(tokens);
  statement curr;
  statement_vec statements;
  // will have to fiddle with this in benchmarks
  statements.reserve(token_list.size() / 3);
  for (auto &t : token_list) {
    if (t == "\n") {
      if (!curr.empty()) {
        statements.emplace_back(std::move(curr));
        curr.clear();
      }
    } else if (t == "{" || t == "}" || t == ",") {
      if (!curr.empty()) {
        statements.emplace_back(std::move(curr));
        curr.clear();
      }
      statements.emplace_back(statement{std::move(t)});
    } else {
      curr.emplace_back(std::move(t));
      t.clear();
    }
  }
  return statements;
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
namespace cplang {
expression_list lex(const string &in) {
  // who knows how many copy constructors this thing calls
  // need to optimize later TODOS
  auto a = in | mtx::pipe([](auto &&f) {
             preprocess(f);
             return f;
           }) |
           mtx::pipe([](auto &&f) { return token_parse(f); }) |
           mtx::pipe([](auto &&f) {
             return statement_parse(std::forward<token_vec>(f));
           }) |
           mtx::pipe([](auto &&in) { return parse(in); });
  using namespace std::literals;
}
} // namespace cplang
  /* */