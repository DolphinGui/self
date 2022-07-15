#pragma once
#include <utility>
namespace self {
template <typename T> class pair_range {
  std::pair<T, T> range;

public:
  pair_range(std::pair<T, T> range) : range(range) {}
  auto begin() { return range.first; }
  auto end() { return range.second; }
};
} // namespace self