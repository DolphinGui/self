#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace selflang{
  using string = std::string;
  using string_view = std::string_view;
  template<typename type>
  using vector = std::vector<type>;
}