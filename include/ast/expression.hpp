#pragma once
#include <cstddef>
#include <functional>
#include <memory>
#include <ostream>
#include <string_view>
#include <vector>

namespace self {
using Token = std::string;
using TokenView = std::string_view;
struct Type {
  virtual TokenView getTypename() const noexcept = 0;
};
enum struct RefTypes { value, ref, ptr, any };
template <typename T> class TypeRefBase {
public:
  T ptr;
  // These are qualifiers.
  RefTypes is_ref = RefTypes::any;
  TypeRefBase(T ptr) : ptr{ptr} {}
  TypeRefBase(T ptr, RefTypes ref) : ptr{ptr}, is_ref(ref) {}
  friend bool operator==(TypeRefBase left, TypeRefBase right) {
    using enum RefTypes;
    if (!left.ptr || !right.ptr)
      return false;
    if (left.is_ref == any || right.is_ref == any) {
      return left.ptr == right.ptr;
    }
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
};
using TypePtr = TypeRefBase<const Type *>;
using TypeRef = TypeRefBase<const Type &>;

struct Expression {
  virtual ~Expression() = default;
  virtual bool isComplete() const { return true; }
  virtual TypePtr getType() const noexcept { return {nullptr}; }
  virtual std::ostream &print(std::ostream &) const = 0;
  friend std::ostream &operator<<(std::ostream &os, const Expression &ex) {
    return ex.print(os);
  }
  virtual TokenView getName() const noexcept = 0;
  virtual bool isCompiletime() const noexcept { return false; }
};

using ExpressionPtr = std::unique_ptr<Expression>;
using ExpressionRef = std::reference_wrapper<Expression>;
using ExpressionConstRef = std::reference_wrapper<const Expression>;
using ExpressionList = std::vector<ExpressionPtr>;

} // namespace self