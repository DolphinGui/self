#pragma once
#include "ast/expression_tree.hpp"
#include "builtins.hpp"
#include <istream>
namespace self {
using TokenVec = std::vector<Token>;
namespace detail {
std::string preprocess(std::string contents);
TokenVec parseToken(std::string whole);
} // namespace detail
ExprTree lex(std::string in, Context &c);
} // namespace self