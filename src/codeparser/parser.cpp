#include "parser.hpp"
#include "ast/control.hpp"
#include "ast/tuple.hpp"
#include "builtins.hpp"
#include <span>

namespace self {
namespace detail::parser {

bool coerceType(self::ExprPtr &e, self::TypePtr type) {
  if (auto *var = dynamic_cast<self::VarDeclaration *>(e.get())) {
    auto t = var->getRawName();
    if (!var->type_ref.ptr) {
      var->type_ref = type;
      if (type.depth == 0)
        throw std::runtime_error("var coercion depth should not be 0");
      --var->type_ref.depth;
      if (var->type_ref.depth == 0)
        var->type_ref.is_ref = self::RefTypes::value;
      return true;
    }
  } else if (auto *uneval =
                 dynamic_cast<self::UnevaluatedExpression *>(e.get())) {
    uneval->coerced_type = type;
    return true;
  }
  return e->getType() == type;
}

void GlobalParser::process(TokenIt t, self::ExprTree &syntax_tree,
                           self::Index &global) {
  using namespace self::reserved;
retry:
  try {
    while (!t.end()) {
      if (*t == var_t) {
        auto name = *++t;
        syntax_tree.push_back(parseVar(++t, name, global));
      } else if (*t == fun_t) {
        auto name = *++t;
        errReport(notReserved(name), t.coord(), "function name is reserved");
        auto f = parseFun(++t, name, global);
        if (f->body_defined) {
          f->qualifiers = self::Qualifiers::qExport;
        }
        syntax_tree.push_back(std::move(f));
      } else if (notReserved(*t)) {
        syntax_tree.push_back(parseExpr(t, global));
      } else if (*t == import_t) {
        processImport(++t, syntax_tree, global);
      } else if (*t == "extern") {
        processExtern(++t, syntax_tree);
      } else {
        errReport(self::reserved::isEndl(*t), t, "invalid expression");
      }
    }
  } catch (std::runtime_error e) {
    err.errors.push_back(self::Error{t.col, t.line, e.what()});
    while (*t != ";")
      ++t;
    goto retry;
  }
}

self::ExprTree GlobalParser::process(TokenIt t, self::Index &i) {
  self::ExprTree syntax_tree;
  process(t, syntax_tree, i);
  syntax_tree.shrink_to_fit();
  return syntax_tree;
}

coerceResult typeCoercible(self::Context &c, self::TypePtr to,
                           self::TypePtr from) {
  using enum coerceResult;
  if (to.ptr == from.ptr) {
    if (to.depth == from.depth)
      return match;
    if (to.depth - 1 == from.depth)
      return coerce;
  }
  if (from.ptr == &c.deduced_t && to.depth - 1 == from.depth) {
    return match;
  }
  return mismatch;
}

coerceResult needCoerce(self::Context &c, self::ExprBase *e,
                        self::TypePtr type) {
  using enum coerceResult;
  if (auto *var = dynamic_cast<self::VarDeclaration *>(e)) {
    if (!var->getDecltype().ptr) {
      return coerce;
    } else if (var->getDecltype() == type) {
      return match;
    }
  } else if (const auto *uneval =
                 dynamic_cast<const self::UnevaluatedExpression *>(e)) {
    return coerce;
  }

  return typeCoercible(c, e->getType(), type);
}

std::unique_ptr<self::FunctionDef>
GlobalParser::parseFun(TokenIt &t, self::TokenView name, self::Index &parent) {
  auto curr = self::makeExpr<self::FunctionDef>(t.coord(), name, parent);
  expectToken("(", t, "'(' expected");
  while (*t != ")") {
    errReport(notReserved(*t), t,
              "reserved Token cannot be used as parameter name");
    curr->arguments.emplace_back(
        self::makeExpr<self::VarDeclaration>(t.coord(), *t++));
    expectToken("(", t, "':' expected here");
    constexpr auto commaOrParen = [](self::TokenView t) {
      return t == "," || t == ")";
    };
    auto e = parseExpr(t, parent, nullptr, commaOrParen);
    auto type = self::getLiteralType(*e);
    curr->arguments.back()->type_ref = {&type.ptr, type.is_ref};
    errReport(*t == ")" || *t == ",", t, "')' or ',' expected");
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
            errReport(type != ret->getRetType(c), t,
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
} // namespace detail::parser

Module parse(LexedFileRef &in, Context &c) {
  ErrorList e;
  std::vector<const self::StructDef *> list;
  auto parser = detail::parser::GlobalParser(c, e, list);
  auto root = Index(c.root);
  auto ast = parser.process(detail::parser::TokenIt{in}, root);
  return Module(std::move(root), std::move(ast), std::move(e), std::move(list));
}
} // namespace self