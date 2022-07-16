#pragma once

#include "ast/expression_tree.hpp"
#include "lexer.hpp"

namespace self {
ExpressionTree parse(TokenVec in, Context& c);

}