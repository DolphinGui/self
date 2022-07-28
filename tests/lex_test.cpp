#include <fmt/core.h>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string_view>

#define SELF_FMT_FORMATTABLE
#include "builtins.hpp"
#include "lexer.hpp"

constexpr auto reference = "var a = 21; var b = ref(a)";
constexpr auto while_inline =
    "fun b()->i64; fun function(){var a = false\n while a; b();}";
constexpr auto do_while =
    "fun function(){var a = false; do var b = 32;while a;}";
constexpr auto if_else =
    "fun function(){ var a = true; if a; var b = 2;else var b = 4; }";
constexpr auto if_else_block =
    "fun function(){ var a = true; if a{ var b = 2;} }";
constexpr auto elif = "fun function(){ var a = true; if a{ if a{var b = 3;}} }";
constexpr auto boolean =
    "var a = true; var b = false; var c = 2 == 3; var d = 2 != 3";
constexpr auto deref = "var a = 5; var b = a";
constexpr auto var_decl = "var a: i64";
constexpr auto expr = "var a = 1 + ((2 - 5 + 6) / 3) * 2";
constexpr auto function_def = "fun main()->i64{return 0;}";
constexpr auto foward_decl = "fun function()->i64;";
constexpr auto struct_test = "var b = i64;var a = struct{var a: i64;}";
constexpr auto nesting = "var a = struct{fun a()->i64{return 22;};var b = 2;}";
constexpr auto import_test = "extern \"C\" import \"../stdlib/include/io.h\""
                             "fun main()->i64{"
                             "  return 0;"
                             "}";
int main() {
  uint count = 0;
  self::Context c;
  for (auto file : {reference, while_inline, do_while, if_else_block, if_else,
                    elif, nesting, boolean, struct_test, import_test,
                    function_def, expr, deref, var_decl, foward_decl}) {
    fmt::print("Test {}:\n", count++);
    auto f = std::string(file);
    auto results = self::parseFile(f, c);
    for (auto &ex : results.ast) {
      fmt::print("{}", *ex);
    }
    if (results.ast.isComplete()) {
      fmt::print("\ntype is complete\n");
    } else {
      fmt::print("\ntype is not complete\n");
    }
    fmt::print("{}\n\n", results.errs);
  }
}