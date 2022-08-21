#include "backend_config.hpp"
#include "builtins.hpp"
#include "lexer.hpp"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fmt/core.h>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <limits>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string_view>

constexpr auto path = "a.o";
constexpr auto output_name = "a.out";
// this is not crossplatform in the slightest.
// I will have to figure out how to
// query OS details about this later. but for now, this'll do
constexpr auto link_command =
    "ld -pie --eh-frame-hdr -dynamic-linker "
    "/lib64/ld-linux-x86-64.so.2 /usr/lib64/Scrt1.o /usr/lib64/crti.o "
    "{} {} -lc "
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

std::string readFile() {
  std::string contents;
  auto file = std::ifstream("../examples/test.me", std::ios::binary);
  file.ignore(std::numeric_limits<std::streamsize>::max());
  auto size = file.gcount();
  contents.reserve(size);
  file.seekg(std::ios::beg);
  std::copy_n(std::istreambuf_iterator(file), size,
              std::back_inserter(contents));
  return contents;
}

int main() {
  self::Context c;
  llvm::LLVMContext llvm;
  llvm.enableOpaquePointers();
  auto stdlib = get_stdlib();
  auto f = readFile();
  auto AST = self::parseFile(f, c);
  std::cout << AST.ast;
  if (!AST.errs.errors.empty()) {
    std::cerr << AST.errs << '\n';
    std::exit(0);
  }
  auto [IR, di] = self::codegen(AST, c, llvm, "../examples/test.me");
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