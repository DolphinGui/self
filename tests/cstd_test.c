#include "io.h"
#include <string.h>

const char *string = "hello world\n";
int main() {
  セルフprimative_strview hello_world = {strlen(string),
                                         (const unsigned char *)string};
  selfprint(&hello_world);
}