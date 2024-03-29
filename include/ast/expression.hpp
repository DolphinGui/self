#pragma once
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <functional>
#include <memory>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <vector>

#ifdef SELF_FMT_FORMATTABLE
#include <fmt/ostream.h>
#endif

namespace self {
using Token = std::string;
using TokenView = std::string_view;
struct Type {
  virtual TokenView getTypename() const noexcept = 0;
  friend bool operator==(const Type &lhs, const Type &rhs) {
    return &lhs == &rhs;
  }
};
enum struct RefTypes { value, ref };
struct TypeRef {
  const Type &ptr;
  // These are qualifiers.
  RefTypes is_ref = RefTypes::value;
  uint8_t depth = 0;
  bool is_mutable = false;
  TypeRef(const Type &ptr) : ptr{ptr} {}
  TypeRef(const Type &ptr, RefTypes ref) : ptr{ptr}, is_ref(ref) {
    if (ref == RefTypes::ref)
      depth = 1;
  }
  TypeRef(const Type &ptr, RefTypes ref, uint8_t depth, bool is_mutable = false)
      : ptr{ptr}, is_ref(ref), depth(depth), is_mutable(is_mutable) {}
  friend bool operator==(TypeRef left, TypeRef right) {
    using enum RefTypes;
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
  std::string dump() const {
    std::string result;
    result.append(ptr.getTypename());
    auto i = depth;
    while (i--) {
      result.push_back('*');
    }
    return result;
  }
};

struct TypePtr {
  const Type *ptr;
  // These are qualifiers.
  RefTypes is_ref = RefTypes::value;
  uint8_t depth = 0;
  bool is_mutable = false;
  TypePtr() = default;
  TypePtr(const Type *ptr) : ptr{ptr} {}
  TypePtr(const Type *ptr, RefTypes ref) : ptr{ptr}, is_ref(ref) {
    if (ref == RefTypes::ref)
      depth = 1;
  }
  TypePtr(const Type *ptr, RefTypes ref, uint8_t depth, bool is_mutable = false)
      : ptr{ptr}, is_ref(ref), depth(depth), is_mutable(is_mutable) {}
  TypePtr(TypeRef t)
      : ptr{&t.ptr}, is_ref(t.is_ref), depth(t.depth),
        is_mutable(t.is_mutable) {}
  friend bool operator==(TypePtr left, TypePtr right) {
    using enum RefTypes;
    if (!left.ptr || !right.ptr)
      return false;
    return left.ptr == right.ptr && left.is_ref == right.is_ref;
  }
  operator TypeRef() const {
    if (!ptr) {
      throw std::runtime_error("TypePtr is null");
    }
    return {*ptr, is_ref};
  }
  std::string dump() const {
    std::string result;
    result.append(ptr->getTypename());
    auto i = depth;
    while (i--) {
      result.push_back('*');
    }
    return result;
  }
  bool operator!() const noexcept { return !ptr; }
};

struct Pos {
  size_t col = 0, line = 0;
};

enum struct Qualifiers : unsigned char {
  none,
  qImport,
  qExport,
};

struct ExprBase;
struct ExprVisitor;
using ExprPtr = std::unique_ptr<ExprBase>;
struct ExprBase {
  Pos pos;
  Qualifiers qualifiers = Qualifiers::none;
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
  virtual void visit(ExprVisitor &, void *data) const = 0;
  bool isExternal() const noexcept {
    return qualifiers == Qualifiers::qImport ||
           qualifiers == Qualifiers::qExport;
  }
};

template <typename T, typename... Args>
inline std::unique_ptr<T> makeExpr(Pos pos, Args &&...args) {
  static_assert(std::is_base_of_v<ExprBase, T>, "T must derive from ExprBase");
  auto result = std::make_unique<T>(std::forward<Args>(args)...);
  result->pos = pos;
  return result;
}

template <typename T> inline std::unique_ptr<T> makeExpr(Pos pos) {
  static_assert(std::is_base_of_v<ExprBase, T>, "T must derive from ExprBase");
  auto result = std::make_unique<T>();
  result->pos = pos;
  return result;
}

inline auto cloneif(const ExprPtr &e) {
  if (e) {
    return e->clone();
  } else {
    return ExprPtr(nullptr);
  }
}

#ifdef SELF_FMT_FORMATTABLE
}
template <> struct fmt::formatter<self::ExprBase> : fmt::ostream_formatter {};
namespace self {
#endif

namespace detail {
template <class, class = void> struct canVisit_impl : std::false_type {};
template <class T>
struct canVisit_impl<
    T, std::void_t<decltype(std::declval<ExprVisitor>()(std::declval<T>()))>>
    : std::true_type {};
template <typename T> inline constexpr bool canVisit = canVisit_impl<T>::value;
}; // namespace detail

template <typename Derive> struct ExprImpl : public ExprBase {
  ExprPtr clone() const override {
    return std::make_unique<Derive>(static_cast<const Derive &>(*this));
  }
  void visit(ExprVisitor &guest, void *data) const override {
    guest(static_cast<const Derive &>(*this), data);
  }
};

template <typename Derive> struct NameQualification {
  static std::string qualify(std::string_view t) {
    std::string name = Derive::prefix;
    name.append(t);
    return name;
  }
  static std::string dequalify(std::string_view t) {
    auto result = std::string(t);
    return std::string(t.substr(std::strlen(Derive::prefix)));
  }
  Token getRawName() const noexcept {
    return dequalify(static_cast<const Derive &>(*this).getName());
  }
};
using ExprRef = std::reference_wrapper<ExprBase>;
using ExprConstRef = std::reference_wrapper<const ExprBase>;
using ExpressionList = std::vector<ExprPtr>;
using SymbolMap = std::unordered_multimap<TokenView, ExprRef>;
class Index;
enum struct FullyResolved : bool { Resolved, Unresolved };
std::pair<ExprPtr, FullyResolved> foldExpr(ExprPtr &&, Index &local);
} // namespace self