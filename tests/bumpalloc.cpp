#include "utility/bumpalloc.hpp"
#include "gtest/gtest.h"

struct nontriviallydestructable {
  ~nontriviallydestructable() {
    // haha does nothing
  }
};

TEST(bumpalloc, allocint) {
  auto allocator = self::Bumpalloc(256);
  auto *arr = allocator.makeArr<int, 321>(2);
  // this shouldn't compile
  //   auto *b = allocator.makeArr<nontriviallydestructable, 321>();
  EXPECT_EQ(arr[12], 2);
}
