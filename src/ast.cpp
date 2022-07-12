#include "ast/functions.hpp"
#include "ast/variables.hpp"

namespace selflang{
  std::ostream & fun_def_base::print(std::ostream &out) const {
    out << "fun " << name;
    if (!arguments.empty()) {
      out << '(';
      for (auto &arg : arguments) {
        out << *arg << ',';
      }
      out<< ')';
    }
    if (return_type) {
      out << " -> " << *return_type;
    }
    if (!body.empty()) {
      out << "{";
      for (auto &p : body) {
        out << *p.get();
      }
      out << '}';
    }
    return out;
  }
}