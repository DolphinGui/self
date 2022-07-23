#include "io.h"
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

int64_t selfflush() { return fputc('\n', stdout); }
uint64_t selfprint(const セルフprimative_strview *s) {
  for (uint64_t i = 0; i != s->size; ++i) {
    int err = fputc((int)(s->str[i]), stdout);
    if (!err) {
      return err;
    }
  }
  return 0;
}
uint64_t printmulti(unsigned char c, uint64_t n) {
  for (int64_t i = 0; i < n; i++) {
    int result = fputc((int)c, stdout);
    if (errno) {
      fputs(strerror(result), stderr);
      return errno;
    }
  }
  return 0;
}
int64_t geterrno() { return errno; }
// haha I lied
int64_t randomnumber() { return 6; }