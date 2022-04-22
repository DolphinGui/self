#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <iostream>
#include <sstream>
#include <string_view>
constexpr std::string_view var_decl_test = "var a: byte\nvar b: void";
constexpr std::string_view type_decl = "var a = 1 + 2";
int main() {
  for (const auto &file : {var_decl_test}) {
    std::string string(file);
    auto results = selflang::lex(string);
    for (auto &ex : results) {
      std::cout << *ex << '\n';
    }
  }
}