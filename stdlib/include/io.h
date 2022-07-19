#pragma once
#include <stdint.h>
#include <stdint.h>
struct str_view_t {
  unsigned char *str;
  uint64_t size;
};
typedef struct str_view_t str_view;
uint64_t selfputchar(unsigned char c);
uint64_t selfputcharerr(unsigned char c);
uint64_t selfprint(str_view s);
uint64_t printmulti(unsigned char c, int64_t n);
uint64_t geterrno();