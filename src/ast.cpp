#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include <ast/tuple.hpp>
#include <cstddef>
#include <memory>
#include <optional>
#include <stdexcept>

namespace self {
std::pair<ExprPtr, FullyResolved> foldExpr(ExprPtr &&e, Index &local) {
  using enum FullyResolved;
  if (auto *call = dynamic_cast<FunctionCall *>(e.get())) {
    auto f = std::unique_ptr<FunctionCall>(
        dynamic_cast<FunctionCall *>(e.release()));
    switch (f->definition.internal) {
    case detail::call:
      // todo implement compiletime function evaluation later
      return {std::move(f), Unresolved};
    case detail::store:
    case detail::assign:
    case detail::addi:
    case detail::subi:
    case detail::muli:
    case detail::divi:
    case detail::cmpeq:
    default:
      // not sure if there's a point to folding constant arithmetic
      return {std::move(f), Unresolved};
    }
  } else if (auto *deref = dynamic_cast<VarDeref *>(e.get())) {
    auto result = local.find(deref->getName());
    auto var = dynamic_cast<const VarDeclaration *>(&result->get());
    if (dynamic_cast<Literal *>(var->value)) {
      return {e->clone(), Resolved};
    }
    return {var->value->clone(), Unresolved};
  } else {
    return {std::move(e), Unresolved};
  }
}
} // namespace self