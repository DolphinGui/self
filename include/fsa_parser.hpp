#pragma once

#include "ast/expression_tree.hpp"
#include "lexer.hpp"

namespace self {
ExprTree parse(TokenVec in, Context& c);

}