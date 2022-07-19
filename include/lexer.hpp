#pragma once
#include "ast/expression_tree.hpp"
#include "builtins.hpp"
#include <istream>
namespace self {
using TokenVec = std::vector<Token>;
ExprTree lex(std::string in, Context& c);
} // namespace self