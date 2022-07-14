#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "lexer.hpp"
using namespace std::string_view_literals;
constexpr auto var_decl = "var a: byte"sv;
constexpr auto expr = "var a = 1 + ((2 - 5 + 6) / 3) * 2"sv;
constexpr auto fun_def = "fun main()->i32{return 0;}"sv;
constexpr auto foward_decl = "fun function()->char;"sv;
constexpr auto struct_test = "var a = struct{var a: char;}"sv;
constexpr auto deref = "var a = 5;var b = a"sv;
// constexpr auto tuple = "var b = (5 - 2 + 2, \"3\", '3')"sv;
int main() {
  uint count = 0;
  for (const auto &file : {struct_test, var_decl, expr, fun_def, foward_decl}) {
    fmt::print("Test {}:\n", count++);
    auto results = selflang::lex(std::string(file));
    for (auto &ex : results) {
      std::cout << *ex << '\n';
    }
    if (results.isComplete()) {
      std::cout << "type is complete\n";
    } else {
      std::cout << "type is not complete\n";
    }
  }
}