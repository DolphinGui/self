#include <fmt/core.h>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string_view>

#define SELF_FMT_FORMATTABLE
#include "builtins.hpp"
#include "lexer.hpp"

constexpr auto while_inline =
    "fun b()->i64; fun var(){var a = false; while a; b();}";
int main() {
  uint count = 0;
  self::Context c;
  std::cout << std::setw(4);
  for (auto file : {while_inline}) {
    fmt::print("Test {}:\n", count++);
    auto f = std::string(file);
    auto results = self::parseFile(f, c);
    for (auto &ex : results.ast) {
      fmt::print("{}\n", *ex);
    }
    if (results.ast.isComplete()) {
      fmt::print("type is complete\n");
    } else {
      fmt::print("type is not complete\n");
    }
    if (!results.errs.errors.empty()) {
      std::cout << results.errs << '\n';
    }
  }
}