#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <list>

namespace selflang {
using string = std::string;
using string_view = std::string_view;
template <typename type> using vector = std::vector<type>;
template <typename type> using list = std::list<type>;
template <typename... types> using variant = std::variant<types...>;
} // namespace selflang