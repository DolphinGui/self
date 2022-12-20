#pragma once

#include <cstddef>
#include <new>
#include <type_traits>
#include <utility>

namespace self {
class Bumpalloc {
  struct Node {
    char *memory;
    char *ptr;
    Bumpalloc *other;
    size_t size;
  } node;
  constexpr static Node nullnode = {};
  Bumpalloc *head;

  auto getHead(size_t size) {
    if (head->node.ptr + size + sizeof(Bumpalloc) > head->node.memory) {
      auto newsize = head->node.size * 2;
      if (newsize < size)
        newsize = size;
      auto newhead = new (head->node.ptr) Bumpalloc(newsize);
      head->node.other = newhead;
      head = newhead;
    }
    return head;
  }

public:
  Bumpalloc(size_t expected_size) {
    node.memory = static_cast<char *>(::operator new(expected_size));
    node.ptr = node.memory;
    head = this;
    node.other = nullptr;
    node.size = expected_size;
  }
  Bumpalloc(const Bumpalloc &) = delete;
  Bumpalloc(Bumpalloc &&other) {
    node = other.node;
    head = other.head;
    other.node = nullnode;
    other.head = nullptr;
  }

  template <typename T> T *make(auto &&...params) {
    static_assert(
        std::is_trivially_destructible_v<T>,
        "this cannot keep track of non-trivially destructable things");
    auto head = getHead(sizeof(T));
    auto pointer =
        new (head->node.ptr) T(std::forward<decltype(params)...>(params...));
    head->node.ptr += sizeof(T);
    return pointer;
  }

  template <typename T> T *make() {
    static_assert(
        std::is_trivially_destructible_v<T>,
        "this cannot keep track of non-trivially destructable things");
    auto head = getHead(sizeof(T));
    auto pointer = new (head->node.ptr) T();
    head->node.ptr += sizeof(T);
    return pointer;
  }

  template <typename T, size_t num> T *makeArr(auto &&...params) {
    static_assert(
        std::is_trivially_destructible_v<T>,
        "this cannot keep track of non-trivially destructable things");
    auto head = getHead(sizeof(T) * num);
    auto arr =
        new (head->node.ptr) T(std::forward<decltype(params)...>(params...));
    for (int i = 1; i <= num; ++i) {
      new (head->node.ptr) T(std::forward<decltype(params)...>(params...));
      head->node.ptr += sizeof(T);
    }
    return arr;
  }

  template <typename T, size_t num> T *makeArr() {
    static_assert(
        std::is_trivially_destructible_v<T>,
        "this cannot keep track of non-trivially destructable things");
    auto head = getHead(sizeof(T) * num);
    auto arr = new (head->node.ptr) T();
    for (int i = 1; i <= num; ++i) {
      new (head->node.ptr) T();
      head->node.ptr += sizeof(T);
    }
    return arr;
  }

  ~Bumpalloc() {
    if (node.other) {
      node.other->~Bumpalloc();
    }
    ::operator delete(node.memory);
  }
};
} // namespace self