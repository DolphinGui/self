#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "lexer.hpp"

constexpr std::string_view var_decl_test = "var a: byte";
constexpr std::string_view expr_test = "var a = 1 + ((2 - 5) / 3) * 2";
constexpr std::string_view fun_def_test = "\
fun main()->int{return 0;}";
int main() {
  uint count = 0;
  for (const auto &file : {expr_test}) {
    fmt::print("Test {}:\n", count++);
    auto results = selflang::lex(std::string(file));
    for (auto &ex : results) {
      std::cout << *ex << '\n';
    }
    if (results.is_complete()) {
      std::cout << "type is complete\n";
    } else {
      std::cout << "type is not complete\n";
    }
  }
}