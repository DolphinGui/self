#include "lexer.hpp"
#include <climits>
#include <ios>
#include <istream>
#include <re2/re2.h>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>

namespace {
using namespace cplang;

void remove_comments(std::string &contents) {
  RE2::Replace(&contents, "\\/\\/.+?\\n", " ");
  RE2::Replace(&contents, "\\/\\*[\\S\\s]+?\\*\\/", " ");
}

void for_parse(std::string &whole, auto callback) {
  re2::StringPiece input(whole);
  std::string token;
  while (RE2::FindAndConsume(
      &input, "([^\\s(){}\\[\\];\"]+|\\(|\\)|{|}|\\[|\\]|(?:\".+\")|\\n)",
      &token)) {
    callback(token);
  }
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

} // namespace
/* statement types:
var decl: typename var
struct def: struct typename{ stuff }*/
namespace cplang {
expression_list lex(std::istream &in) {
  auto file = read_file(in);
  remove_comments(file);
  expression_list syntax_tree;
  
  for_parse(file, [&](auto& string){

  });
  return syntax_tree;
}
} // namespace cplang