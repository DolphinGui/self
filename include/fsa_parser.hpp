#pragma once

#include "lexer.hpp"
#include <boost/sml.hpp>
#include <syntax_tree.hpp>

namespace cplang {
expression_list parse(statement_vec &in);

}