#include "io.h"
#include <stdio.h>

int selfputchar(unsigned char c) { return fputc(c, stdout); }

int selfputcharerr(unsigned char c) { return fputc(c, stderr); }
int selfflush() { return fputc('\n', stdout); }