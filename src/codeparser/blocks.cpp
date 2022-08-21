#include "parser.hpp"

#include "ast/control.hpp"

using namespace self::detail::parser;
namespace {

std::unique_ptr<self::Block> forceBlock(GlobalParser &g, TokenIt &t,
                                        self::Block &parent) {
  if (*t == "{") {
    ++t;
    return self::makeExpr<self::Block>(t.coord(),
                                       g.parseBlock(t, parent.contexts));
  }
  auto results = self::makeExpr<self::Block>(t.coord(), parent.contexts);
  results->push_back(g.parseExpr(t, results->contexts));
  return results;
}

void parseIf(GlobalParser &g, TokenIt &t, self::Block &body) {
  ++t;
  auto if_statement = self::makeExpr<self::If>(t.coord());

  if_statement->condition =
      g.parseExpr(t, body.contexts, nullptr, [](self::TokenView t) -> bool {
        return t == ";" || t == "{";
      });
  errReport(if_statement->condition->getType().ptr == &g.c.bool_t, t.coord(),
            "if condition expression is supposed to be boolean.");
  consumeNullExpr(t);
  if_statement->block = forceBlock(g, t, body);
  if (*t.next() == self::reserved::else_t) {
    ++ ++t;
    consumeNullExpr(t);
    if_statement->else_block = forceBlock(g, t, body);
  }
  body.push_back(std::move(if_statement));
}

void parseWhile(GlobalParser &g, TokenIt &t, self::Block &body) {
  self::ExprPtr condition;
  auto p = t.coord();
  if (*t == self::reserved::while_t) {
    ++t;
    condition =
        g.parseExpr(t, body.contexts, nullptr, [](self::TokenView t) -> bool {
          return t == ";" || t == "{";
        });
    if (*t == ";")
      ++t;
  } else {
    // assumes *t == "do"
    ++t;
  }
  auto block = forceBlock(g, t, body);
  bool is_do = false;
  if (condition == nullptr) {
    is_do = true;
    ++t;
    errReport(*t == self::reserved::while_t, t,
              "expected a 'while' after a do");
    condition = g.parseExpr(t, body.contexts);
    ++t;
  }
  body.push_back(self::makeExpr<self::While>(p, std::move(block),
                                             std::move(condition), is_do));
}
} // namespace
namespace self::detail::parser {

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
      parseIf(*this, t, body);
    } else if (*t == while_t || *t == do_t) {
      parseWhile(*this, t, body);
    } else if (notReserved(*t)) {
      body.push_back(parseExpr(t, body.contexts));
    } else {
      ++t;
    }
  }
  ++t;
  return body;
}
} // namespace self::detail::parser