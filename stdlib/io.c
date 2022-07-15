#include "io.h"
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

int selfputchar(unsigned char c) { return fputc((int)c, stdout); }

int selfputcharerr(unsigned char c) { return fputc((int)c, stderr); }
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

int printmulti(unsigned char c, int64_t n) {
  for (int64_t i = 0; i < n; i++) {
    int result = selfputchar(c);
    if (errno) {
      fputs(strerror(result), stderr);
      return errno;
    }
  }
  return 0;
}
int geterrno() { return errno; }