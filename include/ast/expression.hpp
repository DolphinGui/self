#pragma once
#include <functional>
#include <memory>
#include <ostream>
#include <string_view>
#include <vector>

namespace selflang {
using token = std::string;
using token_view = std::string_view;
enum struct ref_types { value, ref, ptr, any };
class var_decl;
template <typename T> class type_indirect {
public:
  T ptr;
  // These are qualifiers.
  ref_types is_ref = ref_types::value;
  friend bool operator==(type_indirect left, type_indirect right) {
    using enum ref_types;
    if (!left.ptr || !right.ptr)
      return false;
    if (left.is_ref == any || right.is_ref == any) {
      return left.ptr == right.ptr;
    }
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
};
using type_ptr = type_indirect<const var_decl *>;
using type_ref = type_indirect<const var_decl &>;

struct expression {
  virtual ~expression() = default;
  virtual bool isComplete() const { return true; };
  friend std::ostream &operator<<(std::ostream &os, expression const &ex) {
    return ex.print(os);
  }
  virtual type_ptr getType() const noexcept { return {nullptr}; }
  virtual std::ostream &print(std::ostream &) const = 0;
  virtual token_view getName() const noexcept = 0;
};

using expression_ptr = std::unique_ptr<expression>;
using expression_ref = std::reference_wrapper<expression>;
using expression_const_ref = std::reference_wrapper<const expression>;
using expression_list = std::vector<expression_ptr>;
} // namespace selflang