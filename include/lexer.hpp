#pragma once
#include "syntax_tree.hpp"
#include <istream>
namespace cplang {
using token_vec = vector<token>;
using statement = token_vec;
using statement_vec = vector<statement>;
using type_ref = type *;
using type_list = vector<type_ref>;
using symbol_ref = symbol *;
using symbol_list = vector<symbol_ref>;
expression_list lex(const string &in);
} // namespace cplang