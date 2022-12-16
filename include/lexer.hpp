#pragma once
#include "ast/expression_tree.hpp"
#include "builtins.hpp"
#include <istream>
namespace self {
using TokenVec = std::vector<Token>;
struct LexedFileRef {
  TokenVec tokens;
  std::string &file;
};
namespace detail {
std::string &preprocess(std::string &contents);
LexedFileRef parseToken(std::string &whole);
} // namespace detail
Module parseFile(std::string &in, Context &c);
} // namespace self