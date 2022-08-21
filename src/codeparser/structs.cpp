#include "parser.hpp"

namespace {

void configStruct(std::unique_ptr<self::StructLit> &str) {
  std::vector<std::unique_ptr<self::VarDeclaration>> args;
  for (auto &member : str->value.body) {
    if (auto *var_decl = dynamic_cast<self::VarDeclaration *>(member.get())) {
      args.push_back(std::make_unique<self::VarDeclaration>(*var_decl));
    }
  }
  args.shrink_to_fit();
  auto ctor = std::make_unique<self::FunctionDef>(
      "struct", *str->value.context, str->value, true, std::move(args));
  ctor->return_type = {str->value};
  str->value.insert(std::move(ctor));
  str->value.insert(std::make_unique<self::OperatorDef>(
      "=", self::TypeRef{str->value, self::RefTypes::ref},
      self::VarDeclarationPtr("this",
                              self::TypeRef(str->value, self::RefTypes::ref)),
      self::VarDeclarationPtr("value", str->value), *str->value.context,
      self::detail::store, true));
  str->value.body.shrink_to_fit();
}
using namespace self::detail::parser;
std::unique_ptr<self::StructLit> makeStruct(GlobalParser &g, TokenIt &t,
                                            self::Index &parent) {
  static unsigned int id = 0;
  auto identity = std::string("struct");
  identity.append(std::to_string(id++));
  errReport(*t == "{" || *t == "(", t.coord(), "Expected a \"{\" or \"(\"");
  auto p = t.coord();
  if (*t == "(") {
    ++t;
    if (*t == ")") {
      ++t;
      return self::makeExpr<self::StructLit>(p, self::StructDef(0, parent),
                                             g.c);
    } else {
      auto size = isInt(*t);
      errReport(size.has_value(), t, "Expected integer or \")\"");
      return self::makeExpr<self::StructLit>(p, self::StructDef(*size, parent),
                                             g.c);
    }
  } else {
    auto result = self::StructDef(parent);
    ++t;
    while (*t != "}") {
      if (*t == "var") {
        auto name = *++t;
        result.body.push_back(g.parseVar(++t, name, *result.context));
      } else if (*t == "fun") {
        auto name = *++t;
        errReport(notReserved(name), t, "function name is reserved");
        result.body.push_back(g.parseFun(t, name, *result.context));
      } else {
        errReport(self::reserved::isEndl(*t), t,
                  "expected a function or variable declaration.");
      }
    }
    ++t;
    result.identity = id;
    auto ret = self::makeExpr<self::StructLit>(p, std::move(result), g.c);
    configStruct(ret);
    return ret;
  }
}
} // namespace
namespace self::detail::parser {

std::unique_ptr<self::StructLit>
GlobalParser::parseStruct(TokenIt &t, self::Index &parent) {
  auto str = makeStruct(*this, t, parent);
  struct_list.push_back(&str->value);
  return str;
}
} // namespace self::detail::parser