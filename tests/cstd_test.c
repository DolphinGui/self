#include "io.h"
#include <string.h>

const char *string = "hello world\n";
int main() {
  for (const char *c = string; *c != '\0'; ++c) {
    selfputchar(*c);
  }
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wincompatible-pointer-types-discards-qualifiers"
  str_view hello_world = {string, strlen(string)};
#pragma clang diagnostic pop
  selfprint(hello_world);
}