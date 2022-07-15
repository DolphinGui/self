#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "lexer.hpp"
using namespace std::string_view_literals;
constexpr auto VarDeclaration = "var a: i64"sv;
constexpr auto expr = "var a = 1 + ((2 - 5 + 6) / 3) * 2"sv;
constexpr auto FunctionDef = "fun main()->i64{return 0;}"sv;
constexpr auto foward_decl = "fun function()->char;"sv;
constexpr auto struct_test = "var a = struct{var a: char;}"sv;
constexpr auto deref = "var a = 5;var b = a"sv;
// constexpr auto tuple = "var b = (5 - 2 + 2, \"3\", '3')"sv;
int main() {
  uint count = 0;
  for (const auto &file : {struct_test, VarDeclaration, expr, FunctionDef, foward_decl}) {
    fmt::print("Test {}:\n", count++);
    auto results = self::lex(std::string(file));
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