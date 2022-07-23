#pragma once

#include "expression.hpp"
#include <iterator>
#include <utility>

namespace self {
class Index {
private:
  SymbolMap curr;
  Index *parent = nullptr;
  Index() = default;

public:
  explicit Index(Index &parent) noexcept : parent(&parent) {}
  Index(const Index &other) : parent(other.parent) {}
  static Index createRoot() { return Index(); }

  auto equal_range(TokenView t) const {
    auto result = curr.equal_range(t);
    if (result.first != result.second)
      return result;
    if (parent)
      return parent->equal_range(t);
    return std::pair{curr.end(), curr.end()};
  }
  bool isRoot() const noexcept { return parent; }
  bool isUnique(TokenView t) const {
    auto count = curr.count(t);
    if (count == 1)
      return true;
    if (parent && count == 0)
      return parent->isUnique(t);
    return false;
  }
  std::optional<ExprConstRef> find(TokenView t) {
    auto r = equal_range(t);
    if (std::distance(r.first, r.second) != 1)
      return std::nullopt;

    return {r.first->second};
  }
  void insert(std::pair<TokenView, ExprConstRef> &&pair) {
    curr.insert(std::forward<std::pair<TokenView, ExprConstRef>>(pair));
  }
  bool contains(TokenView t) const {
    if (curr.contains(t)) {
      return true;
    }
    if (parent)
      return parent->contains(t);
    return false;
  }
};
} // namespace self