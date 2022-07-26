#pragma once
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <system_error>

namespace self {

inline auto createtmp(std::string &out) {
  out = std::tmpnam(nullptr);
  std::error_code err;
  auto result = std::make_unique<llvm::raw_fd_ostream>(out, err);
  if (err)
    throw std::system_error(err);
  return result;
}
} // namespace self