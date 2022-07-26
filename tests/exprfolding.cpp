#include "fmt/core.h"
#include <array>
#include <cstdio>
#include <filesystem>
#include <iostream>

std::string getFilename(std::FILE *file) {
  namespace fs = std::filesystem;
  std::stringstream path;

  path << fs::read_symlink(fs::path("/proc/self/fd") /
                           std::to_string(fileno(file)));
  return path.str();
}

int main() {
  auto tmp = std::tmpfile();
  auto data = std::array{'a', 'b', 'c', 'd', '\n'};
  std::fwrite(data.data(), sizeof(char), data.size(), tmp);
  std::string path = getFilename(tmp);
  std::fseek(tmp, 0, SEEK_SET);
  std::string a;
  a.resize(data.size());
  std::fread(a.data(), sizeof(char), data.size(), tmp);
  std::cout << a << ", " << path << '\n';
  auto command = fmt::format("cat {}", path);
  std::system(command.c_str());
}