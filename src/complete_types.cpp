#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"

namespace {
void dispatch(self::ExpressionRef e) {
  if (auto *tree = dynamic_cast<self::ExpressionTree *>(&e.get())) {
    tree->complete_types();
  } else if (auto *fun = dynamic_cast<self::FunBase *>(&e.get())) {
    fun->body.complete_types();
  }
}
} // namespace

void self::ExpressionTree::complete_types() {
  for (auto &e : *this) {
    dispatch(*e);
  }
}