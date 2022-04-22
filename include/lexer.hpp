#pragma once
#include "syntax_tree.hpp"
#include <istream>
namespace selflang {
using token_vec = vector<token>;
using statement = token_vec;
using statement_vec = vector<statement>;
expression_list lex(const string &in);
} // namespace cplang