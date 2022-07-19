#include "fmt/core.h"
#include <argh.h>
#include <clang-c/CXString.h>
#include <clang-c/Index.h>
#include <cstdio>
#include <fmt/format.h>
#include <iostream>
#include <vector>
constexpr auto path = "../stdlib/include/io.h";

auto convertString(CXString str) {
  std::string result = clang_getCString(str);
  clang_disposeString(str);
  return result;
}
struct Variable {
  std::string name;
  std::string type;
  void print() const { fmt::print("var {}: {}\n", name, type); }
};
struct FunctionPrototype {
  std::string name;
  std::string ret;
  std::vector<Variable> args;
  void print() const {
    fmt::print("fun {} (", name);
    for (const auto &arg : args) {
      fmt::print("{}: {},", arg.name, arg.type);
    }
    fmt::print(") -> {}\n", ret);
  }
};
struct Struct {
  std::string name;
  std::vector<Variable> members;
  void print() const {
    fmt::print("struct {}\n", name);
    for (const auto &member : members) {
      fmt::print("  var {}: {}\n", member.name, member.type);
    }
  }
};
struct Context {
  std::vector<Variable> global_variables;
  std::vector<FunctionPrototype> functions;
  std::vector<Struct> types;
  void printStructs() const {
    for (const auto &type : types) {
      type.print();
    }
  }
  void printFunctions() const {
    for (const auto &fun : functions) {
      fun.print();
    }
  }
  void printVars() const {
    for (const auto &var : global_variables) {
      var.print();
    }
  }
};

auto listStruct(CXCursor cursor, CXCursor parent, CXClientData c) {
  Context &context = *static_cast<Context *>(c);
  auto name = convertString(clang_getCursorSpelling(cursor));
  auto type = convertString(clang_getTypeSpelling(clang_getCursorType(cursor)));
  context.types.back().members.push_back(Variable{name, type});
  return CXChildVisit_Continue;
}
void listArg(CXCursor cursor, CXClientData c) {
  Context &context = *static_cast<Context *>(c);
  auto name = convertString(clang_getCursorSpelling(cursor));
  auto type = convertString(clang_getTypeSpelling(clang_getCursorType(cursor)));
  context.functions.back().args.push_back(Variable{name, type});
}
int main() {
  Context c;
  CXIndex index = clang_createIndex(0, 0);
  CXTranslationUnit unit = clang_parseTranslationUnit(
      index, path, nullptr, 0, nullptr, 0, CXTranslationUnit_None);
  if (unit == nullptr) {
    fmt::print(stderr, "Unable to parse translation unit. Quitting.\n");
    std::exit(-1);
  }
  CXCursor cursor = clang_getTranslationUnitCursor(unit);
  clang_visitChildren(
      cursor,
      [](CXCursor cursor, CXCursor parent, CXClientData c) {
        Context &context = *static_cast<Context *>(c);
        switch (clang_getCursorKind(cursor)) {
        case CXCursor_StructDecl: {
          context.types.push_back(
              Struct{convertString(clang_getCursorSpelling(cursor))});
          clang_visitChildren(cursor, &listStruct, c);
          break;
        }
        case CXCursor_FunctionDecl: {
          context.functions.push_back(FunctionPrototype{
              .name = convertString(clang_getCursorSpelling(cursor)),
              .ret = convertString(
                  clang_getTypeSpelling(clang_getCursorType(cursor)))});
          auto argc = clang_Cursor_getNumArguments(cursor);
          for (auto i = 0; i < argc; ++i) {
            listArg(clang_Cursor_getArgument(cursor, i), c);
          }
          break;
        }
        default:
          break;
        }
        return CXChildVisit_Continue;
      },
      &c);
  clang_disposeTranslationUnit(unit);
  clang_disposeIndex(index);
  c.printStructs();
  c.printFunctions();
}