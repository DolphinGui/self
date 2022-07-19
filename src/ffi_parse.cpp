#include "ffi_parse.hpp"
#include <clang-c/CXString.h>
#include <clang-c/Index.h>
#include <iostream>
#include <stdexcept>

namespace {
std::string convertString(CXString s) {
  std::string results = clang_getCString(s);
  clang_disposeString(s);
  return results;
}

} // namespace

namespace self {
void ffParse(ExprTree &in, std::string_view path) {

  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit unit;
  auto err =
      clang_parseTranslationUnit2(index, path.data(), nullptr, 0, nullptr, 0,
                                  CXTranslationUnit_SkipFunctionBodies, &unit);
  if (err) {
    throw std::runtime_error("FFI parsing error");
  }
}
} // namespace self