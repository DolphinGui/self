#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <fmt/core.h>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <sstream>
#include <string_view>

constexpr auto file = "\
\
fun main()->int{return 1;}";

int main() {
  auto AST = selflang::lex(file);
  std::cout << AST << '\n';
  auto &IR = AST.codegen();
  IR.print(llvm::outs(), nullptr);
}