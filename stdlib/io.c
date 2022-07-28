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
int64_t geterrno() { return errno; }
// haha I lied
int64_t randomnumber() { return 6; }

int64_t printnum(int64_t *number) {
  fprintf(stdout, "%ld\n", *number);
  return 0;
}