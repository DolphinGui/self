#include "backend_config.hpp"
#include "lexer.hpp"
#include "syntax_tree.hpp"

#include <cstdlib>
#include <fmt/core.h>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <sstream>
#include <string_view>

constexpr auto file = "\
\
fun main()->int{return 1;}";
constexpr auto path = "a.o";
constexpr auto output_name = "a.out";

int main() {
  auto AST = selflang::lex(file);
  std::cout << AST << '\n';
  auto &IR = AST.codegen();
  IR.print(llvm::outs(), nullptr);
  std::error_code file_err;
  auto aout = llvm::raw_fd_ostream(path, file_err);
  if (file_err) {
    llvm::errs() << "Could not open file: " << file_err.message();
    return 1;
  }
  selflang::compile(IR, aout);
  auto command = fmt::format("ld {} -o{}", path, output_name);
  std::system(command.c_str());
}