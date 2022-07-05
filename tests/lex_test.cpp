#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

constexpr std::string_view var_decl_test = "var a: byte\nvar b: void";
constexpr std::string_view expr_test = "var a = 1 + (2 - 5 / 3) * 2";
constexpr std::string_view fun_def_test = "fun a(){var b = 2;}";
int main() {
  uint count = 0;
  for (const auto &file : {fun_def_test}) {
    fmt::print("Test {}:\n", count++);
    std::string string(file);
    auto results = selflang::lex(string);
    for (auto &ex : results) {
      std::cout << *ex << '\n';
    }
  }
}