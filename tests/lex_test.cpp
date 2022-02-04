#include "lexer.hpp"
#include "syntax_tree.hpp"
#include <iostream>
#include <sstream>
#include <string_view>
constexpr std::string_view file = "byte a\n";
int main(){
  std::string string(file);
  cplang::name_list types = {"byte"};
  auto results = cplang::lex(string, types);
  for(auto& ex : results){
    std::cout << *ex << '\n';
  }
}