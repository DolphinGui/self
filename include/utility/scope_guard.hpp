#pragma once

#include <utility>

namespace self {
template <typename Construct, typename Destruct> class scope_guard {
  Destruct d;

public:
  scope_guard(const Construct &c, const Destruct &d) : d(d) { c(); }
  scope_guard(Construct &&c, Destruct &&d) : d(std::move(d)) { c(); }
  scope_guard(Destruct d) : d(d) {}
  scope_guard(Destruct &&d) : d(std::move(d)) {}
  ~scope_guard() { d(); }
  static_assert(noexcept(d()), "destructor may not throw");
};
}; // namespace self