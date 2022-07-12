#pragma once
#include <memory>
#include <ostream>
#include <string_view>
#include <vector>

namespace selflang {
using token = std::string;
using token_view = std::string_view;
// there are declaration statements, and evaluative statements.
// I think pointers to pointers are stupid
// and maybe I'll figure out something
// that fragments memory less
// until then this'll just be slow
// would love to do this with variant types
struct expression {
  virtual ~expression() = default;
  virtual bool is_complete() const { return true; };
  virtual void complete_types() {}
  friend std::ostream &operator<<(std::ostream &os, expression const &ex) {
    return ex.print(os);
  }
  virtual std::ostream &print(std::ostream &) const = 0;
  virtual token_view getName() const noexcept = 0;
};

using expression_ptr = std::unique_ptr<expression>;
using expression_list = std::vector<expression_ptr>;
} // namespace selflang