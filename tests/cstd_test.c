#include "io.h"
#include <string.h>

const char *string = "hello world!\n";
int main() {
  セルフprimative_strview hello_world = {13, (const unsigned char *)string};
  selfprint(&hello_world);
}