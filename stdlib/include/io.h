#pragma once
#include <stdint.h>
struct str_view {
  unsigned char *str;
  uint64_t size;
};
typedef struct str_view str_view;
uint64_t selfputchar(unsigned char c);
uint64_t selfputcharerr(unsigned char c);
uint64_t printmulti(unsigned char c, uint64_t n);
int64_t geterrno();
int64_t selfflush();