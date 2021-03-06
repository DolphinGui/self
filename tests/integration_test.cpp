#include "backend_config.hpp"
#include "builtins.hpp"
#include "lexer.hpp"

#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fmt/core.h>
#include <iostream>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string_view>
// for some reason raw string does not include
// newline. In a real file the semicolons are
// not necessary.
constexpr auto file = "extern \"C\" import \"../stdlib/include/io.h\"\n"
                      "fun main()->i64{\n"
                      "  var a = 123\n"
                      "  var b = ref(a)\n"
                      "  printnum(a)\n"
                      "  printnum(b)\n"
                      "  return 0\n"
                      "}";
constexpr auto path = "a.o";
constexpr auto output_name = "a.out";
// this is not crossplatform in the slightest.
// I will have to figure out how to
// query OS details about this later. but for now, this'll do
constexpr auto link_command =
    "ld -pie --eh-frame-hdr -dynamic-linker "
    "/lib64/ld-linux-x86-64.so.2 /usr/lib64/Scrt1.o /usr/lib64/crti.o "
    "/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtbeginS.o "
    "-L/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0 "
    "-L/usr/lib64 -L/lib64 -L/lib -L/usr/lib {} {} -lc "
    "/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtendS.o "
    "/usr/lib64/crtn.o -o {}";
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
  self::Context c;
  llvm::LLVMContext llvm;
  llvm.enableOpaquePointers();
  auto stdlib = get_stdlib();
  auto f = std::string(file);
  auto AST = self::parseFile(f, c);
  std::cout << AST.ast;
  auto IR = self::codegen(AST.ast, c, llvm);
  std::error_code file_err;
  IR->print(llvm::outs(), nullptr);
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