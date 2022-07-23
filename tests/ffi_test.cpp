#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ffi_parse.hpp"
#include <filesystem>
#include <fmt/format.h>
#include <fstream>
#include <sstream>
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)
constexpr auto flags =
    "-D_GNU_SOURCE  -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS "
    "-D__STDC_LIMIT_MACROS  -I{0}/stdlib/include -g";
int main() {
  auto f = fmt::format(flags, STR(PROJECT_SOURCE));
  self::ExprTree tree;
  self::Context c;
  auto working = std::filesystem::current_path();
  auto ioheader =
      working.parent_path().append("stdlib").append("include").append("io.h");
  self::parseFFI(tree, c.root, c, ioheader.c_str(), f);
  fmt::print("{}", tree.dump());
}