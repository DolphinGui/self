#include "parser.hpp"

using namespace self::detail::parser;
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
      auto [success, size] = isInt(*t++);
      errReport(success, t.coord(), "Expected integer or \")\"");
      return self::makeExpr<self::StructLit>(p, self::StructDef(size, parent),
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
        errReport(notReserved(name), t.coord(), "function name is reserved");
        result.body.push_back(g.parseFun(++t, name, *result.context));
      } else {
        errReport(self::reserved::isEndl(*t), t.coord(),
                  "expected a function or variable declaration.");
        ++t;
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
std::unique_ptr<self::FunctionDef>
GlobalParser::parseFun(TokenIt &t, self::TokenView name, self::Index &parent) {
  auto curr = self::makeExpr<self::FunctionDef>(t.coord(), name, parent);
  errReport(*t == "(", t.coord(), "\"(\" expected");
  ++t;
  while (*t != ")") {
    errReport(notReserved(*t), t.coord(),
              "reserved Token cannot be used as parameter name");
    curr->arguments.emplace_back(
        self::makeExpr<self::VarDeclaration>(t.coord(), *t++));
    errReport(*t == ":", t.coord(), "\":\" expected here");
    ++t;
    constexpr auto commaOrParen = [](self::TokenView t) {
      return t == "," || t == ")";
    };
    auto e = parseExpr(t, parent, nullptr, commaOrParen);
    auto type = self::getLiteralType(*e);
    curr->arguments.back()->type_ref = {&type.ptr, type.is_ref};
    errReport(*t == ")" || *t == ",", t.coord(), "\")\" or \",\" expected");
    ++t;
    if (*t == ")")
      break;
    else
      ++t;
  }
  if (*++t == "->") {
    ++t;
    constexpr auto commaOrBracket = [](self::TokenView t) {
      return self::reserved::isEndl(t) || t == "{";
    };
    auto e = parseExpr(t, parent, nullptr, commaOrBracket);
    auto type = self::getLiteralType(*e);
    curr->return_type = {&type.ptr, type.is_ref};
    curr->body_defined = false;
  }
  if (*t == "{") {
    ++t;
    curr->body.emplace(parseBlock(t, parent, [&](self::Block &b) {
      for (auto &arg : curr->arguments) {
        b.contexts.insert({arg->getName(), *arg});
      }
    }));
    curr->body_defined = true;
    if (curr->return_type.ptr == nullptr) {
      self::TypePtr type{};
      for (auto &expr : *curr->body) {
        if (auto ret = dynamic_cast<self::Ret *>(expr.get())) {
          if (!type.ptr) {
            type = ret->getRetType(c);
          } else {
            errReport(type != ret->getRetType(c), t.coord(),
                      "Cannot deduce return type of function.");
          }
        }
      }
      if (type.ptr == nullptr) {
        curr->return_type.ptr = &c.void_t;
        if (!dynamic_cast<self::Ret *>(curr->body->back().get())) {
          curr->body->push_back(self::makeExpr<self::Ret>(t.coord()));
        }
      } else {
        curr->return_type = type;
      }
    }
  }
  parent.insert({curr->name, *curr});
  return curr;
}

void GlobalParser::parseIf(TokenIt &t, self::Block &body) {
  ++t;
  auto if_statement = self::makeExpr<self::If>(t.coord());

  if_statement->condition =
      parseExpr(t, body.contexts, nullptr,
                [](self::TokenView t) -> bool { return t == ";" || t == "{"; });
  errReport(if_statement->condition->getType().ptr == &c.bool_t, t.coord(),
            "if condition expression is supposed to be boolean.");
  consumeNullExpr(t);
  if_statement->block = forceBlock(t, body);
  if (*t.next() == self::reserved::else_t) {
    ++ ++t;
    consumeNullExpr(t);
    if_statement->else_block = forceBlock(t, body);
  }
  body.push_back(std::move(if_statement));
}

void GlobalParser::parseWhile(TokenIt &t, self::Block &body) {
  self::ExprPtr condition;
  auto p = t.coord();
  if (*t == self::reserved::while_t) {
    ++t;
    condition =
        parseExpr(t, body.contexts, nullptr, [](self::TokenView t) -> bool {
          return t == ";" || t == "{";
        });
    if (*t == ";")
      ++t;
  } else {
    // assumes *t == "do"
    ++t;
  }
  auto block = forceBlock(t, body);
  bool is_do = false;
  if (condition == nullptr) {
    is_do = true;
    ++t;
    errReport(*t == self::reserved::while_t, t.coord(),
              "expected a 'while' after a do");
    ++t;
    condition = parseExpr(t, body.contexts);
    ++t;
  }
  body.push_back(self::makeExpr<self::While>(p, std::move(block),
                                             std::move(condition), is_do));
}

self::ExprPtr GlobalParser::parseVar(TokenIt &t, self::TokenView name,
                                     self::Index &context) {
  using namespace self::reserved;
  if (!notReserved(name)) {
    std::stringstream err;
    err << "Token " << name << " is reserved";
    errReport(false, t.coord(), err.str());
  }
  auto curr = self::makeExpr<self::VarDeclaration>(t.coord(), name);
  if (*t == ":") {
    ++t;
    auto expr = parseExpr(t, context);
    auto type = self::getLiteralType(*expr);
    curr->type_ref = {&type.ptr, type.is_ref};
    context.insert({curr->getName(), std::ref(*curr)});
    return curr;
  } else {
    errReport(notReserved(*t), t.coord(),
              "non-reserved Token expected in expression");
    context.insert({curr->getName(), std::ref(*curr)});
    return parseExpr(t, context, [&](self::ExprTree &tree) {
      tree.push_back(std::move(curr));
    });
  }
}

std::unique_ptr<self::Block> GlobalParser::forceBlock(TokenIt &t,
                                                      self::Block &parent) {
  if (*t == "{") {
    ++t;
    return self::makeExpr<self::Block>(t.coord(),
                                       parseBlock(t, parent.contexts));
  }
  auto results = self::makeExpr<self::Block>(t.coord(), parent.contexts);
  results->push_back(parseExpr(t, results->contexts));
  return results;
};
self::Block
GlobalParser::parseBlock(TokenIt &t, self::Index &parent,
                         std::function<void(self::Block &)> callback) {
  auto body = self::Block(parent);
  if (callback)
    callback(body);
  while (*t != "}") {
    using namespace self::reserved;
    if (*t == var_t) {
      auto name = *++t;
      body.push_back(parseVar(++t, name, body.contexts));
    } else if (*t == return_t) {
      ++t;
      if (!self::reserved::isEndl(*t)) {
        body.push_back(
            self::makeExpr<self::Ret>(t.coord(), parseExpr(t, body.contexts)));
      } else {
        body.push_back(self::makeExpr<self::Ret>(t.coord()));
      }
    } else if (*t == if_t) {
      parseIf(t, body);
    } else if (*t == while_t || *t == do_t) {
      parseWhile(t, body);
    } else if (notReserved(*t)) {
      body.push_back(parseExpr(t, body.contexts));
    } else {
      ++t;
    }
  }
  ++t;
  return body;
}

std::unique_ptr<self::StructLit>
GlobalParser::parseStruct(TokenIt &t, self::Index &parent) {
  auto str = makeStruct(*this, t, parent);
  struct_list.push_back(&str->value);
  return str;
}

} // namespace self::detail::parser