#pragma once

#include <functional>
#include <typeinfo>
#include <unordered_map>

#include "ast/expression.hpp"
#include "ast/variables.hpp"

namespace selflang {
using var_decl_ref = std::reference_wrapper<var_decl>;
namespace detail {
inline std::unordered_map<size_t, const var_decl_ref> global_typeid_builtin_map;
}
struct builtin_type : public var_decl {
  builtin_type(token_view name, type_ref type, const std::type_info &key)
      : var_decl(name, type) {
    detail::global_typeid_builtin_map.insert({key.hash_code(), *this});
  }
  builtin_type(token_view name) : var_decl(name) {}
  void register_type(type_ref type, const std::type_info &key) {
    this->type.ptr = &type.ptr;
    this->type.is_ref = type.is_ref;
    detail::global_typeid_builtin_map.insert({key.hash_code(), *this});
  }
};

template <typename T> struct literal : public expression {
  T value;
  var_ref type;
  inline std::ostream &print(std::ostream &out) const override {
    return out << "literal: " << value;
  }
  inline token_view getName() const noexcept override {
    return "literal value";
  };
  literal(T value)
      : literal(value,
                {detail::global_typeid_builtin_map.at(typeid(T).hash_code())}) {
  }

private:
  literal(T itself, var_ref type) : value(itself), type(type){};
};
} // namespace selflang