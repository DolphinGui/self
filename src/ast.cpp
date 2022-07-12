#include "ast/functions.hpp"
#include "ast/variables.hpp"

namespace selflang{
  std::ostream & fun_def_base::print(std::ostream &out) const {
    out << "function: " << name;
    if (return_type) {
      out << " returns " << *return_type;
    }

    if (!arguments.empty()) {
      out << "\nargs: ";
      for (auto &arg : arguments) {
        out << *arg << '\n';
      }
    }
    if (!body.empty()) {
      out << "\nbody:\n";
      for (auto &p : body) {
        out << *p.get();
      }
    }
    return out;
  }
}