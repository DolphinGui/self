#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include <ast/tuple.hpp>
#include <cstddef>
#include <memory>
#include <stdexcept>

namespace {} // namespace

namespace self {

std::pair<ExpressionPtr, FullyResolved>
folder(ExpressionPtr &&e, SymbolMap &local, SymbolMap &global, Context &c) {
  using enum FullyResolved;
  const auto doIntOp =
      [&](std::unique_ptr<FunctionCall> &&fun,
          auto binary) -> std::pair<ExpressionPtr, FullyResolved> {
    auto f = std::move(fun);
    auto lhs = folder(std::move(f->lhs), local, global, c),
         rhs = folder(std::move(f->rhs), local, global, c);
    if (lhs.second == Resolved && rhs.second == Resolved) {
      auto &left = dynamic_cast<IntLit &>(*lhs.first);
      auto &right = dynamic_cast<IntLit &>(*rhs.first);
      return {std::make_unique<IntLit>(binary(left.value, right.value), c),
              Resolved};
    } else {
      if (lhs.second == Resolved) {
        f->lhs = std::move(lhs.first);
      } else if (rhs.second == Resolved) {
        f->rhs = std::move(rhs.first);
      }
      return {std::move(f), Unresolved};
    }
  };
  if (auto *call = dynamic_cast<FunctionCall *>(e.get())) {
    auto f = std::unique_ptr<FunctionCall>(
        dynamic_cast<FunctionCall *>(e.release()));
    switch (f->definition.internal) {
    case detail::call:
      // todo implement compiletime function evaluation later
      return {std::move(f), Unresolved};
    case detail::store:
    case detail::assign: {
      auto &var = dynamic_cast<VarDeclaration &>(*call->lhs.get());
      var.value = call->rhs.get();
      return {std::move(f), Unresolved};
    }
    case detail::addi:
    case detail::subi:
    case detail::muli:
    case detail::divi:
      // not sure if there's a point to folding constant arithmetic
      return {std::move(f), Unresolved};
    }
  } else if (auto *deref = dynamic_cast<VarDeref *>(e.get())) {
    auto result = local.find(deref->getName());
    if (result == local.end()) {
      return {std::move(e), Unresolved};
    }
    auto var = dynamic_cast<const VarDeclaration *>(&result->second.get());
    if (dynamic_cast<Literal *>(var->value)) {
      return {e->clone(), Resolved};
    }
    return {var->value->clone(), Unresolved};
  } else {
    return {std::move(e), Unresolved};
  }
}
} // namespace self