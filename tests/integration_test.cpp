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
fun selfputchar(c: char)->int\n \
fun main()->int{selfputchar('h');return 0;}";
constexpr auto path = "a.o";
constexpr auto output_name = "a.out";
// this is not crossplatform in the slightest.
// I will have to figure out how to
// query OS details about this later. but for now, this'll do
constexpr auto link_command =
    R"(ld -pie --eh-frame-hdr -m elf_x86_64 -dynamic-linker \
 /lib64/ld-linux-x86-64.so.2 /usr/lib64/Scrt1.o /usr/lib64/crti.o \
 /usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtbeginS.o -L/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0 \
 -L/usr/lib64 -L/lib64 -L/lib -L/usr/lib {} -lc /usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtendS.o \
 /usr/lib64/crtn.o -o {})";
int main() {
  auto AST = selflang::lex(file);
  std::cout << AST << '\n';
  auto &IR = selflang::codegen(AST);
  IR.print(llvm::outs(), nullptr);
  std::error_code file_err;
  auto aout = llvm::raw_fd_ostream(path, file_err);
  if (file_err) {
    llvm::errs() << "Could not open file: " << file_err.message();
    return 1;
  }
  selflang::compile(IR, aout);
  auto command = fmt::format(link_command, path, output_name);
  // std::system(command.c_str());
}