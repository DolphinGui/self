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
    case detail::addi:
    case detail::subi:
    case detail::muli:
    case detail::divi:
    case detail::cmpeq:
    default:
      // not sure if there's a point to folding constant arithmetic
      return {std::move(f), Unresolved};
    case detail::assign:
    case detail::store:
      if (auto *var = dynamic_cast<VarDeclaration *>(f->lhs.get())) {
        var->value = f->rhs.get();
      } else if (auto *var = dynamic_cast<VarDeref *>(f->lhs.get())) {
        var->definition.value = f->rhs.get();
      }
      return {std::move(f), Unresolved};
    }
  } else {
    return {std::move(e), Unresolved};
  }
}
} // namespace self