#pragma once
#include "syntax_tree.hpp"
#include <istream>
namespace cplang{
  expression_list lex(std::istream& in);
}