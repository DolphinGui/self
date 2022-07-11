#include <argh.h>
#include <exception>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

int main(int argc, char **argv) {
  auto parse = argh::parser(argc, argv);
  parse.add_param("as");
  parse.parse(argc, argv);
  if (!parse(1)) {
    std::cout << "no input files given\n";
  }else{
    auto file = std::ifstream(parse[1]);
    if(file.fail())
      throw std::runtime_error("failed to open file");
  }
}