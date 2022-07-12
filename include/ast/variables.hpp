#pragma once

#include "ast/expression.hpp"
#include <unordered_map>

namespace selflang {
class var_decl;
template <typename T> class type_indirect {
public:
  T ptr;
  // These are qualifiers.
  bool is_ref = false;
};
using type_ptr = type_indirect<const var_decl *>;
using type_ref = type_indirect<const var_decl &>;

class var_decl : public expression {
  token name;

public:
  type_ptr type;
  var_decl(token_view name) : name(name), type{nullptr} {}
  var_decl(token_view name, type_ref type) : name(name), type{&type.ptr} {}
  var_decl(token_view name, const var_decl &type) : name(name), type{&type} {}

  token_view getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    if (type.ptr)
      return out << "var " << name << ": " << type.ptr->getName();
    else
      return out << "var  " << name;
  }
  bool is_complete() const override { return type.ptr; }
  void complete_types() override {}
};

// name, type/value
std::unique_ptr<var_decl> var_decl_ptr(auto &&...args) {
  return std::make_unique<var_decl>(std::forward<decltype(args)>(args)...);
}
using type_list = std::unordered_multimap<std::string_view, const var_decl *>;

class var_ref : public expression {
  const var_decl &name;

public:
  var_ref(var_decl &itself) : name(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable dereference: " << name;
  }
  inline std::string_view getName() const noexcept override {
    return "var ref";
  }
};
} // namespace selflang