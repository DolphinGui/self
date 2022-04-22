#include <boost/sml.hpp>
#include <fmt/core.h>
#include <fmt/format.h>
#include <iostream>
#include <memory>
#include <string_view>

namespace {
using view = std::string_view;
using namespace boost::sml;
using namespace boost::sml::literals;
auto init = *"start"_s;
constexpr auto event = boost::sml::event<view>;

struct statemachine {
  using self = statemachine;
  uint level;
  std::shared_ptr<sm<self>> inner;
  // explicit statemachine(uint level) : level(level) {}
  auto operator()() const noexcept {
    using namespace boost::sml;
    // clang-format off
    return make_transition_table(
      init + event[([](auto e){return e == "(";})]= "inner"_s,
      init + event[([](auto e){return e == ")";})] = X,
      init + event[([](auto e){return e != "(" || e != ")";})]/&self::print = init,
      "inner"_s + event[&self::recursive_wrapper] = init
    );
    // clang-format on
  }
  bool recursive_wrapper(view e) {
    if (!inner) {
      inner.reset(new sm<self>(self{level + 1}));
    }
    inner->process_event(e);
    return inner->is(X);
  }
  void print(view e) const {
    for (int i = level; i != 0; i--) {
      fmt::print(" ");
    }
    fmt::print("{}\n", e);
  }
};

} // namespace
using namespace std::literals;
constexpr std::array list = {
    "statement"sv, "("sv, "("sv, "statement"sv, ")"sv, ")"sv,
};
int main() {
  auto a = sm<statemachine>(statemachine{0});
  for (auto sv : list)
    a.process_event(sv);
}