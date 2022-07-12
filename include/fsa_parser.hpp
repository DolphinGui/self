#pragma once

#include "lexer.hpp"
#include "ast/expression_tree.hpp"

namespace selflang {
expression_tree parse(token_vec in);

}