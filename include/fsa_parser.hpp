#pragma once

#include "lexer.hpp"
#include "syntax_tree.hpp"

namespace selflang {
expression_tree parse(token_vec in);

}