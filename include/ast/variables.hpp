#pragma once

#include "ast/expression.hpp"
#include <functional>
#include <unordered_map>

namespace selflang {

class var_decl : public expression {
  token name;

public:
  type_ptr type;
  var_decl(token_view name) : name(name), type{nullptr} {}
  var_decl(token_view name, type_ref type)
      : name(name), type{&type.ptr, type.is_ref} {}
  var_decl(token_view name, const var_decl &type) : name(name), type{&type} {}

  token_view getName() const noexcept override { return name; }
  inline std::ostream &print(std::ostream &out) const override {
    if (type.ptr)
      return out << "var " << name << ": " << type.ptr->getName();
    else
      return out << "var  " << name;
  }
  bool isComplete() const override { return type.ptr; }
  type_ptr getType() const noexcept override {
    auto result = type;
    result.is_ref = ref_types::ref;
    return result;
  }
  type_ptr decl_type() const noexcept { return type; }
};

// name, type/value
std::unique_ptr<var_decl> var_decl_ptr(auto &&...args) {
  return std::make_unique<var_decl>(std::forward<decltype(args)>(args)...);
}
using var_ref = std::reference_wrapper<const var_decl>;
using type_list = std::unordered_multimap<std::string_view, var_ref>;

class var_deref : public expression {
  const var_decl &definition;

public:
  var_deref(const var_decl &itself) : definition(itself){};
  inline std::ostream &print(std::ostream &out) const override {
    return out << "variable dereference: " << definition;
  }
  inline std::string_view getName() const noexcept override {
    return "var ref";
  }
  virtual type_ptr getType() const noexcept override {
    auto type = definition.getType();
    type.is_ref = ref_types::ref;
    return type;
  }
};
} // namespace selflang