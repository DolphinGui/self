#pragma once

#include <cstddef>
#include <ostream>
#include <string>
#include <vector>

#ifdef SELF_FMT_FORMATTABLE
#include <fmt/ostream.h>
#endif


namespace self {
struct Error {
  size_t col, row;
  std::string what;
};
struct ErrorList {
  std::vector<Error> errors;
  friend std::ostream &operator<<(std::ostream &out, const ErrorList &errs) {
    for (auto &err : errs.errors) {
      out << "Error at line " << err.row << ", column " << err.col << ": "
          << err.what << '\n';
    }
    return out;
  }
};
} // namespace self


#ifdef SELF_FMT_FORMATTABLE

template <> struct fmt::formatter<self::ErrorList> : fmt::ostream_formatter {};

#endif