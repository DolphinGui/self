#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <iostream>
#include <sstream>
#include <string_view>
constexpr std::string_view var_decl_test = "byte a\n\
byte b\
";
constexpr std::string_view struct_decl_test = "struct a\n\
a b\
";

int main() {
  for (const auto &file : {var_decl_test, struct_decl_test}) {
    std::string string(file);
    auto results = cplang::lex(string);
    for (auto &ex : results) {
      std::cout << *ex << '\n';
    }
  }
}