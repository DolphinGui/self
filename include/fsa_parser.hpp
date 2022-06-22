#pragma once

#include "lexer.hpp"
#include <boost/sml.hpp>
#include "syntax_tree.hpp"

namespace selflang {
expression_list parse(token_vec &in);

}