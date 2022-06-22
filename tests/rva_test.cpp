#include <fmt/core.h>
#include <fmt/format.h>
#include <rva/variant.hpp>
#include <type_traits>
#include <typeinfo>
#include <vector>

#include <cxxabi.h>
template <typename T> std::string type_name() {
  int status = 0;

  std::unique_ptr<char, void (*)(void *)> res{
      abi::__cxa_demangle(typeid(T).name(), NULL, NULL, &status), std::free};

  if (status != 0)
    throw status; // stub

  return res.get();
}

using vec = std::vector<int>;

using variant = rva::variant<vec, std::vector<rva::self_t>>;
using recursive_vec = std::vector<variant>;

int main() {
  vec data;
  recursive_vec recursive;
  variant a = data;
  variant b = recursive;
  rva::visit(
      [](auto &&in) -> void {
        using T = std::decay_t<decltype(in)>;
        fmt::print("{}\n", type_name<T>());
        if constexpr (std::is_same_v<T, recursive_vec>)
          fmt::print("bann\n");
      },
      b);
}