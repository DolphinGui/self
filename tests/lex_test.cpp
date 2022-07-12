#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "lexer.hpp"
using namespace std::string_view_literals;
constexpr auto var_decl = "var a: byte"sv;
constexpr auto expr = "var a = 1 + ((2 - 5) / 3) * 2"sv;
constexpr auto fun_def = "fun main()->int{return 0;}"sv;
constexpr auto foward_decl = "fun function()->char;"sv;
int main() {
  uint count = 0;
  for (const auto &file : {var_decl, expr, fun_def, foward_decl}) {
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