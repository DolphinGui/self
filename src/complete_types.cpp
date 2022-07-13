#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"

namespace {
void dispatch(selflang::expression_ref e) {
  if(auto* tree = dynamic_cast<selflang::expression_tree*>(&e.get())){
    tree->complete_types();
  }else if(auto* fun = dynamic_cast<selflang::fun_def_base*>(&e.get())){
    fun->body.complete_types();
  }
}
} // namespace

void selflang::expression_tree::complete_types() {
  for (auto &e : *this) {
    dispatch(*e);
  }
}