#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"

namespace selflang {
class var_decl;
struct fun_def_base;
struct struct_def : public expression {
  struct_def() = default;
  struct_def(struct_def &&other): body(std::move(other.body)){}
  expression_tree body;
  std::ostream &print(std::ostream &out) const override {
    out << "struct decl: ";
    for (auto &m : body) {
      out << *m << ' ';
    }
    return out;
  }
  token_view getName() const noexcept override { return "struct decl"; };
};
struct opaque_struct : public expression {
  opaque_struct(size_t size = 0) : size(size) {}
  size_t size = 0;
  std::ostream &print(std::ostream &out) const override {
    out << "opaque struct" << size;
    if (size)
      out << " size " << size;
    return out;
  }
  token_view getName() const noexcept override { return "opaque struct"; };
};
} // namespace selflang