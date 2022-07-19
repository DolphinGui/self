#pragma once
typedef unsigned long int uint64_t;
struct str_view {
  unsigned char *str;
  uint64_t size;
};
typedef struct str_view str_view;
uint64_t selfputchar(unsigned char c);
uint64_t selfputcharerr(unsigned char c);
uint64_t selfprint(str_view s);
uint64_t printmulti(unsigned char c, uint64_t n);
uint64_t geterrno();