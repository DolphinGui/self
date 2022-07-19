#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "builtins.hpp"
#include "lexer.hpp"
using namespace std::string_view_literals;
constexpr auto deref = "var a = 5; var b = a"sv;
constexpr auto VarDeclaration = "var a: i64"sv;
constexpr auto expr = "var a = 1 + ((2 - 5 + 6) / 3) * 2"sv;
constexpr auto FunctionDef = "fun main()->i64{return 0;}"sv;
constexpr auto foward_decl = "fun function()->char;"sv;
constexpr auto struct_test = "var b = i64;var a = struct{var a: char;}"sv;
constexpr auto nesting = "var a = struct{fun a()->i64{return 22;}}"sv;
// constexpr auto tuple = "var b = (5 - 2 + 2, \"3\", '3')"sv;
int main() {
  uint count = 0;
  self::Context c;
  for (const auto &file : {FunctionDef, expr, deref, struct_test, nesting,
                           VarDeclaration, foward_decl}) {
    fmt::print("Test {}:\n", count++);
    auto results = self::lex(std::string(file), c);
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