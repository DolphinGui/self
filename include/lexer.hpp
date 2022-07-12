#pragma once
#include "ast/expression_tree.hpp"
#include <istream>
namespace selflang {
using token_vec = std::vector<token>;
using statement = token_vec;
using statement_vec = std::vector<statement>;
expression_tree lex(std::string in);
} // namespace cplang