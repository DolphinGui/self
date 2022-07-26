#pragma once

#include "ast/expression_tree.hpp"
#include "builtins.hpp"
#include "lexer.hpp"

namespace self {
Module parse(LexedFileRef& in, Context &c);
}