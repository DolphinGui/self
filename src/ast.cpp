#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/variables.hpp"
#include <ast/tuple.hpp>

namespace {} // namespace

namespace self {

ExpressionPtr evaluate(ExpressionConstRef e, const SymbolMap &context) {
  if (auto *call = dynamic_cast<const FunctionCall *>(&e.get())) {
    // if(call.h)
  }
}
} // namespace self