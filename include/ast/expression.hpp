#pragma once
#include <cstddef>
#include <cstring>
#include <functional>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace self {
using Token = std::string;
using TokenView = std::string_view;
struct Type {
  virtual TokenView getTypename() const noexcept = 0;
  friend bool operator==(const Type &lhs, const Type &rhs) {
    return lhs.getTypename() == rhs.getTypename();
  }
};
enum struct RefTypes { value, ref, ptr, any };
struct TypeRef {
  const Type &ptr;
  // These are qualifiers.
  RefTypes is_ref = RefTypes::any;
  TypeRef(const Type &ptr) : ptr{ptr} {}
  TypeRef(const Type &ptr, RefTypes ref) : ptr{ptr}, is_ref(ref) {}
  friend bool operator==(TypeRef left, TypeRef right) {
    using enum RefTypes;
    if (left.is_ref == any || right.is_ref == any) {
      return left.ptr == right.ptr;
    }
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
};

struct TypePtr {
  const Type *ptr;
  // These are qualifiers.
  RefTypes is_ref = RefTypes::any;
  TypePtr(const Type *ptr) : ptr{ptr} {}
  TypePtr(const Type *ptr, RefTypes ref) : ptr{ptr}, is_ref(ref) {}
  TypePtr(TypeRef t) : ptr{&t.ptr}, is_ref(t.is_ref) {}
  friend bool operator==(TypePtr left, TypePtr right) {
    using enum RefTypes;
    if (!left.ptr || !right.ptr)
      return false;
    if (left.is_ref == any || right.is_ref == any) {
      return left.ptr == right.ptr;
    }
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
  operator TypeRef() const {
    if (!ptr) {
      throw std::runtime_error("TypePtr is null");
    }
    return {*ptr, is_ref};
  }
};

struct ExprBase;
using ExprPtr = std::unique_ptr<ExprBase>;
struct ExprBase {
  virtual ~ExprBase() = default;
  virtual bool isComplete() const { return true; }
  virtual TypePtr getType() const noexcept { return {nullptr}; }
  virtual std::ostream &print(std::ostream &) const = 0;
  friend std::ostream &operator<<(std::ostream &os, const ExprBase &ex) {
    return ex.print(os);
  }
  virtual TokenView getName() const noexcept = 0;
  virtual bool isCompiletime() const noexcept { return false; }
  virtual ExprPtr clone() const = 0;
};

template <typename Derive> struct ExprImpl : public ExprBase {
  ExprPtr clone() const override {
    return std::make_unique<Derive>(static_cast<const Derive &>(*this));
  }
};

template <typename Derive> struct NameMangling {
  static std::string mangle(std::string_view t) {
    std::string name = Derive::prefix;
    name.append(t);
    return name;
  }
  static std::string demangle(std::string_view t) {
    auto result = std::string(t);
    return std::string(t.substr(std::strlen(Derive::prefix)));
  }
  Token getDemangledName() const noexcept {
    return demangle(static_cast<const Derive &>(*this).getName());
  }
};
using ExprRef = std::reference_wrapper<ExprBase>;
using ExprConstRef = std::reference_wrapper<const ExprBase>;
using ExpressionList = std::vector<ExprPtr>;
using SymbolMap = std::unordered_multimap<TokenView, ExprConstRef>;
class Index;
enum struct FullyResolved : bool { Resolved, Unresolved };
std::pair<ExprPtr, FullyResolved> foldExpr(ExprPtr &&, Index &local);
} // namespace self