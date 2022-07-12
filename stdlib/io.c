#include "io.h"
#include <stdio.h>

int selfputchar(unsigned char c) { return fputc(c, stdout); }

int selfputcharerr(unsigned char c) { return fputc(c, stderr); }
int selfflush() { return fputc('\n', stdout); }

int selfprint(str_view s) {
  for (uint64_t i = 0; i != s.size; ++i) {
    int err = selfputchar(s.str[i]);
    if (!err) {
      return err;
    }
  }
  return 0;
}