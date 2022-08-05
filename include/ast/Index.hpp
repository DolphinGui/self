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

  std::optional<ExprRef> confirm(auto r) {
    if (std::distance(r.first, r.second) != 1)
      return std::nullopt;

    return {r.first->second};
  }

public:
  explicit Index(Index &parent) noexcept : parent(&parent) {}
  Index(const Index &other) : parent(other.parent) {}
  static Index createRoot() { return Index(); }

  auto equalRange(TokenView t) const {
    auto result = curr.equal_range(t);
    if (result.first != result.second)
      return result;
    if (parent)
      return parent->equalRange(t);
    return std::pair{curr.end(), curr.end()};
  }

  auto localEqualRange(TokenView t) const { return curr.equal_range(t); }
  bool isRoot() const noexcept { return parent; }
  bool isUnique(TokenView t) const {
    auto count = curr.count(t);
    if (count == 1)
      return true;
    if (parent && count == 0)
      return parent->isUnique(t);
    return false;
  }
  std::optional<ExprRef> find(TokenView t) { return confirm(equalRange(t)); }
  std::optional<ExprRef> findLocally(TokenView t) {
    return confirm(localEqualRange(t));
  }
  void insert(std::pair<TokenView, ExprRef> &&pair) {
    curr.insert(std::forward<std::pair<TokenView, ExprRef>>(pair));
  }
  bool contains(TokenView t) const {
    if (curr.contains(t)) {
      return true;
    }
    if (parent)
      return parent->contains(t);
    return false;
  }
  bool containsLocally(TokenView t) const { return curr.contains(t); }

  auto begin() { return curr.begin(); }
  auto end() { return curr.end(); }
};
} // namespace self