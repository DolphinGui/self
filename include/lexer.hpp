#pragma once
#include "syntax_tree.hpp"
#include <istream>
namespace selflang {
using token_vec = vector<token>;
using statement = token_vec;
using statement_vec = vector<statement>;
expression_tree lex(string in);
} // namespace cplang