#pragma once
#include <utility>
namespace self {
template <typename T> class pairRange {
  std::pair<T, T> range;

public:
  pairRange(std::pair<T, T> range) : range(range) {}
  auto begin() { return range.first; }
  auto end() { return range.second; }
};
} // namespace self