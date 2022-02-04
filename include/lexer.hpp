#pragma once
#include "syntax_tree.hpp"
#include <istream>
namespace cplang {
expression_list lex(const std::string &in, name_list &typenames);
}