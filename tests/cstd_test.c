#include "io.h"
const char *string = "hello world\n";
int main() {
  for (const char *c = string; *c != '\0'; ++c) {
    selfputchar(*c);
  }
}