#include "backend_config.hpp"
#include "lexer.hpp"

#include <cstdlib>
#include <filesystem>
#include <fmt/core.h>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <sstream>
#include <stdexcept>
#include <string_view>
// for some reason raw string does not include
// newline. In a real file the semicolons are
// not necessary.
constexpr auto file = R"(
fun selfputchar(c: char)->i32;
fun selfflush()->void;
fun printmulti(c: char, i: i32)->i32;
fun geterrno()->i32;
fun main()->i32{
  "this is a string\n";
  var a = 5 - 2;
  var errno = geterrno();
  selfputchar('h');
  selfputchar('e');
  selfputchar('l');
  selfputchar('l');
  selfputchar('o');
  selfputchar(' ');
  selfputchar('w');
  selfputchar('o');
  selfputchar('r');
  selfputchar('l');
  selfputchar('d');
  printmulti('!', 3);
  selfflush();
  return 0;
}

)";
constexpr auto path = "a.o";
constexpr auto output_name = "a.out";
// this is not crossplatform in the slightest.
// I will have to figure out how to
// query OS details about this later. but for now, this'll do
constexpr auto link_command =
    R"(ld -pie --eh-frame-hdr -m elf_x86_64 -dynamic-linker \
 /lib64/ld-linux-x86-64.so.2 /usr/lib64/Scrt1.o /usr/lib64/crti.o \
 /usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtbeginS.o -L/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0 \
 -L/usr/lib64 -L/lib64 -L/lib -L/usr/lib {} {} -lc /usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtendS.o \
 /usr/lib64/crtn.o -o {})";
auto get_stdlib() {
  auto working = std::filesystem::current_path();
  for (auto &f : std::filesystem::directory_iterator(working)) {
    if (f.path().filename() == "libselfstd.a") {
      return f;
    }
  }
  throw std::runtime_error("libselfstd.a not found");
}
int main() {
  auto stdlib = get_stdlib();
  auto AST = self::lex(std::string(file));
  auto IR = self::codegen(AST);
  IR->print(llvm::outs(), nullptr);
  std::error_code file_err;
  auto aout = llvm::raw_fd_ostream(path, file_err);
  if (file_err) {
    llvm::errs() << "Could not open file: " << file_err.message();
    return 1;
  }
  self::compile(*IR, aout);
  auto command =
      fmt::format(link_command, path, stdlib.path().c_str(), output_name);
  std::system(command.c_str());
}