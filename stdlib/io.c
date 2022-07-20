#include "io.h"
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

uint64_t selfputchar(unsigned char c) { return fputc((int)c, stdout); }

uint64_t selfputcharerr(unsigned char c) { return fputc((int)c, stderr); }
int64_t selfflush() { return fputc('\n', stdout); }

uint64_t selfprint(str_view s) {
  for (uint64_t i = 0; i != s.size; ++i) {
    int err = selfputchar(s.str[i]);
    if (!err) {
      return err;
    }
  }
  return 0;
}

uint64_t printmulti(unsigned char c, uint64_t n) {
  for (int64_t i = 0; i < n; i++) {
    int result = selfputchar(c);
    if (errno) {
      fputs(strerror(result), stderr);
      return errno;
    }
  }
  return 0;
}
int64_t geterrno() { return errno; }