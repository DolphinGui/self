#pragma once
#include <stdint.h>
#include <stdint.h>
struct str_view_t {
  unsigned char *str;
  uint64_t size;
};
typedef struct str_view_t str_view;
int selfputchar(unsigned char c);
int selfputcharerr(unsigned char c);
int selfprint(str_view s);
int printmulti(unsigned char c, int64_t n);
int geterrno();