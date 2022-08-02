#include "io.h"
#include <string.h>

const char *string = "hello world!\n";
int main() {
  セルフprimative_strview hello_world = {(const unsigned char *)string, 13};
  selfprint(&hello_world);
}