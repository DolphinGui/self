#include <fmt/core.h>
#include <iostream>
#include <sstream>
#include <string_view>

#include "builtins.hpp"
#include "lexer.hpp"
using namespace std::string_view_literals;
constexpr auto if_else =
    "fun function(){ var a = true; if a; var b = 2;else var b = 4; }"sv;
constexpr auto if_else_block =
    "fun function(){ var a = true; if a{ var b = 2;} }"sv;
constexpr auto elif =
    "fun function(){ var a = true; if a{ if a{var b = 3;}} }"sv;
constexpr auto boolean = "var a = true; var b = false"sv;
constexpr auto deref = "var a = 5; var b = a"sv;
constexpr auto var_decl = "var a: i64"sv;
constexpr auto expr = "var a = 1 + ((2 - 5 + 6) / 3) * 2"sv;
constexpr auto function_def = "fun main()->i64{return 0;}"sv;
constexpr auto foward_decl = "fun function()->i64;"sv;
constexpr auto struct_test = "var b = i64;var a = struct{var a: i64;}"sv;
constexpr auto nesting =
    "var a = struct{fun a()->i64{return 22;};var b = 2;}"sv;
constexpr auto import_test = "extern \"C\" import \"../stdlib/include/io.h\""
                             "fun main()->i64{"
                             "  return 0;"
                             "}"sv;
// constexpr auto tuple = "var b = (5 - 2 + 2, \"3\", '3')"sv;
int main() {
  uint count = 0;
  self::Context c;
  for (const auto &file :
       {if_else_block, if_else, elif, nesting, boolean, struct_test,
        import_test, function_def, expr, deref, var_decl, foward_decl}) {
    fmt::print("Test {}:\n", count++);
    auto results = self::lex(std::string(file), c);
    for (auto &ex : results.ast) {
      std::cout << *ex << '\n';
    }
    if (results.ast.isComplete()) {
      std::cout << "type is complete\n";
    } else {
      std::cout << "type is not complete\n";
    }
  }
}