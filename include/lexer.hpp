#pragma once
#include "ast/expression_tree.hpp"
#include <istream>
namespace self {
using TokenVec = std::vector<Token>;
ExpressionTree lex(std::string in);
} // namespace self