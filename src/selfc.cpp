#define SELF_FMT_FORMATTABLE
#include "argparse/argparse.hpp"
#include "backend_config.hpp"
#include "builtins.hpp"
#include "fmt/core.h"
#include "fmt/format.h"
#include "lexer.hpp"
#include "tmp_ostream.hpp"

#include <algorithm>
#include <clang/Driver/Compilation.h>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <limits>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/Program.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <system_error>
#include <vector>

namespace {
std::string extract(std::string_view path) {
  auto p = std::string(path);
  auto file = std::ifstream(p);
  try {
    file.exceptions(std::ios::failbit);
  } catch (std::ios_base::failure) {
    fmt::print(stderr, "Error with {}: {}\n", path, strerror(errno));
    std::fflush(stderr);
    std::exit(1);
  }
  std::string contents;
  {
    file.ignore(std::numeric_limits<std::streamsize>::max());
    auto size = file.gcount();
    file.clear();
    file.seekg(0, std::ios::beg);
    contents.reserve(size);
    std::copy_n(std::istreambuf_iterator(file), size,
                std::back_inserter(contents));
  }
  return contents;
}
constexpr auto link_command =
    "{} -pie --eh-frame-hdr -dynamic-linker "
    "/lib64/ld-linux-x86-64.so.2 /usr/lib64/Scrt1.o /usr/lib64/crti.o "
    "/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtbeginS.o "
    "-L/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0 "
    "-L/usr/lib64 -L/lib64 -L/lib -L/usr/lib {} -lc "
    "/usr/lib64/gcc/x86_64-pc-linux-gnu/12.1.0/crtendS.o "
    "/usr/lib64/crtn.o -o {}";
} // namespace

namespace arg = argparse;
int main(int argc, char *argv[]) {
  auto args = arg::ArgumentParser("selfc");
  args.add_argument("input")
      .help("The file to be compiled")
      .nargs(arg::nargs_pattern::at_least_one);
  args.add_argument("-o", "--output").default_value(std::string("a.out"));
  args.add_argument("--linker").default_value(std::string("ld"));
  args.parse_args(argc, argv);

  self::Context c;
  llvm::LLVMContext llvm;
  std::vector<std::unique_ptr<llvm::raw_fd_ostream>> tmps;
  std::vector<std::string> objects;

  auto paths = args.get<std::vector<std::string>>("input");
  for (auto &p : paths) {
    if (p.ends_with(".me")) {
      auto file = extract(p);
      auto module = self::parseFile(file, c);
      if (!module.errs.errors.empty()) {
        fmt::print("Errors exist in file {}: {}", p, module.errs);
        std::exit(1);
      }
      auto llvmIR = self::codegen(module.ast, c, llvm, p);
      std::string path;
      tmps.push_back(self::createtmp(path));
      self::compile(*llvmIR, *tmps.back());
      objects.push_back(std::move(path));
    } else {
      objects.push_back(p);
    }
  }

  auto command = fmt::format(link_command, args.get("--linker"),
                             fmt::join(objects, " "), args.get("--output"));
  std::system(command.c_str());
}