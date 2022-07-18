#include "ast/expression.hpp"
#include "lexer.hpp"

#include <iostream>

int main() {
  std::string file = "var a = 3 + 2 - 5";
  self::Context c;
  auto ast = self::lex(file, c);
  std::cout << ast << '\n';
}