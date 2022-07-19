#include "ast/expression.hpp"
#include "ast/functions.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include <ast/tuple.hpp>
#include <cstddef>
#include <memory>
#include <optional>
#include <stdexcept>

namespace {
std::optional<self::ExprConstRef> searchAll(const self::SymbolMap &local,
                                                  const self::SymbolMap &global,
                                                  self::TokenView key) {
  auto a = local.find(key);
  if (a != local.end())
    return {a->second};
  auto b = global.find(key);
  if (b != global.end())
    return {b->second};
  return std::nullopt;
}

} // namespace

namespace self {

std::pair<ExprPtr, FullyResolved>
foldExpr(ExprPtr &&e, SymbolMap &local, SymbolMap &global) {
  using enum FullyResolved;
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
    case detail::cmp:
    default:
      // not sure if there's a point to folding constant arithmetic
      return {std::move(f), Unresolved};
    }
  } else if (auto *deref = dynamic_cast<VarDeref *>(e.get())) {
    auto result = searchAll(local, global, deref->getName());
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