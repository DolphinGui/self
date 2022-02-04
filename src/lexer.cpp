#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <algorithm>
#include <function_pipes.hpp>
#include <iostream>
#include <memory>
#include <re2/re2.h>

namespace {
using namespace cplang;
using raw_statement = std::vector<token>;
void remove_comments(std::string &contents) {
  RE2::Replace(&contents, "\\/\\/.+?\\n", " ");
  RE2::Replace(&contents, "\\/\\*[\\S\\s]+?\\*\\/", " ");
}

auto token_parse(std::string &whole) {
  re2::StringPiece input(whole);
  token cur_token;
  std::vector<token> token_list;
  while (RE2::FindAndConsume(
      &input, "([^\\s(){}\\[\\];\"]+|\\(|\\)|{|}|\\[|\\]|(?:\".+\")|\\n)",
      &cur_token)) {
    token_list.push_back(cur_token);
  }
  return token_list;
}
auto read_file(std::istream &in) {
  in.seekg(std::ios_base::end);
  auto filesize = in.tellg();
  in.seekg(std::ios_base::beg);
  std::string contents;
  contents.reserve(filesize);
  in >> contents;
  return contents;
}

auto statement_parse(auto tokens) {
  std::vector<raw_statement> raw_statements;
  // I'm sure this horribly fragments memory
  // but at this point I really don't care
  // about performance
  raw_statement cur;
  for (int i = 0; i != tokens.size(); i++) {
    if (tokens[i] != "\n") {
      cur.push_back(tokens[i]);
    } else {
      raw_statements.push_back(cur);
      cur.clear();
    }
  }
  return raw_statements;
}

} // namespace
/* statement types:
var decl: typename var
struct def: struct typename{ stuff }*/
namespace cplang {
expression_list lex(const std::string &in, name_list &typenames) {
  // who knows how many copy constructors this thing calls
  // need to optimize later TODOS
  auto statements = in | mtx::pipe([](auto f) {
                      remove_comments(f);
                      return f;
                    }) |
                    mtx::pipe([](auto f) { return token_parse(f); }) |
                    mtx::pipe([](auto t) { return statement_parse(t); });
  expression_list syntax_tree;
  for (auto &statement : statements) {
    if (std::count(typenames.begin(), typenames.end(), statement[0]) != 0) {
      syntax_tree.emplace_back(
          std::make_unique<var_decl>(statement[1], statement[0]));
    }
  }
  return syntax_tree;
}
} // namespace cplang