#include "ffi_parse.hpp"

#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/struct_def.hpp"
#include "ast/variables.hpp"

#include <clang-c/Index.h>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace {
std::string convertString(CXString s) {
  std::string results = clang_getCString(s);
  clang_disposeString(s);
  return results;
}

auto split(std::string &in) {
  std::vector<const char *> pointers;
  unsigned int pos = 0;
  while (pos != in.length()) {
    pointers.push_back(in.data() + pos);
    while (pos != in.length() && in.at(pos) != ' ') {
      ++pos;
    }
    if (pos == in.length())
      break;
    in.at(pos) = '\0';
    ++pos;
    while (pos != in.length() && in.at(pos) == ' ')
      ++pos;
  }
  return pointers;
}
struct Data {
  self::ExprTree &tree;
  self::SymbolMap &symbols;
  self::Context &context;
};

auto getCursorName(CXCursor c) {
  return convertString(clang_getCursorSpelling(c));
}

self::TypePtr getTypePtr(CXType type, self::Context &c) {
  auto name = convertString(clang_getTypeSpelling(type));
  self::RefTypes value_type = self::RefTypes::value;
  if (type.kind == CXTypeKind::CXType_Pointer) {
    value_type = self::RefTypes::ref;
    auto n = clang_getTypeDeclaration(clang_getPointeeType(type));
    name = convertString(clang_getTypeSpelling(clang_getCursorType(n)));
  }
  if (name == "int64_t")
    return {&c.i64_t, value_type};
  else if (name == "uchar")
    return {&c.char_t, value_type};
  else if (name == "uint64_t")
    return {&c.u64_t, value_type};
  else if (name == "struct セルフprimative_strview" ||
           name == "セルフprimative_strview") {
    return {&c.str_t, value_type};
  }
  return nullptr;
}

self::TypeRef getType(CXType type, self::Context &c) {
  auto result = getTypePtr(type, c);
  if (!result.ptr)
    throw std::runtime_error("Type not found");
  else
    return result;
}

auto addArg(CXCursor cursor, self::FunctionDef &fun, self::Context &c) {
  auto name = getCursorName(cursor);
  fun.arguments.push_back(std::make_unique<self::VarDeclaration>(
      name, getType(clang_getCursorType(cursor), c)));
  return CXChildVisit_Continue;
}
struct Data2 {
  self::StructDef &s;
  self::Context &c;
};
auto addStructMember(CXCursor cursor, CXCursor parent, CXClientData d) {
  auto &data = *static_cast<Data2 *>(d);
  auto kind = clang_getCursorKind(cursor);
  auto par = clang_getCursorKind(parent);
  switch (clang_getCursorKind(cursor)) {
  case CXCursor_FieldDecl: {
    data.s.body.push_back(std::make_unique<self::VarDeclaration>(
        getCursorName(cursor), getType(clang_getCursorType(cursor), data.c)));
    break;
  }
  default:
    throw std::runtime_error("unhandled member type");
  }
  return CXChildVisit_Continue;
}
} // namespace

namespace self {
void parseFFI(ExprTree &in, SymbolMap &context, Context &c,
              std::string_view path, std::string flags) {
  CXIndex index = clang_createIndex(0, 0);
  auto pointers = split(flags);
  CXTranslationUnit unit;
  auto err = clang_parseTranslationUnit2(
      index, path.data(), pointers.data(), pointers.size(), nullptr, 0,
      CXTranslationUnit_SkipFunctionBodies, &unit);
  if (err) {
    throw std::runtime_error("FFI parsing error");
  }
  CXCursor cursor = clang_getTranslationUnitCursor(unit);

  Data d = {in, context, c};
  clang_visitChildren(
      cursor,
      [](CXCursor cursor, CXCursor parent,
         CXClientData d) -> CXChildVisitResult {
        Data &data = *static_cast<Data *>(d);
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_FunctionDecl: {
          auto name = getCursorName(cursor);

          auto f = std::make_unique<self::FunctionDef>(name, false);
          f->foreign_name = convertString(clang_Cursor_getMangling(cursor));
          auto argc = clang_Cursor_getNumArguments(cursor);
          auto retType = clang_getResultType(clang_getCursorType(cursor));
          f->return_type = getType(retType, data.context);
          for (auto i = 0; i < argc; ++i) {
            addArg(clang_Cursor_getArgument(cursor, i), *f, data.context);
          }
          data.symbols.insert({f->getName(), std::cref(*f)});
          data.tree.push_back(std::move(f));
          break;
        }
        case CXCursor_StructDecl: {
          auto builtin = getTypePtr(clang_getCursorType(cursor), data.context);
          auto kind = clang_getCursorKind(cursor);
          if (!builtin.ptr) {
            StructDef s;
            s.identity = getCursorName(cursor);
            Data2 d = {s, data.context};
            clang_visitChildren(cursor, &addStructMember, &d);
            data.context.insertForeignType(std::move(s));
          }
          break;
        }
        default:
          break;
        }
        return CXChildVisit_Continue;
      },
      &d);
  clang_disposeTranslationUnit(unit);
  clang_disposeIndex(index);
}
} // namespace self