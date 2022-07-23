#pragma once

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "builtins.hpp"
namespace self {
void parseFFI(ExprTree &in, Index &context, Context &c, std::string_view path,
              std::string flags);
}