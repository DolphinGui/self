#include "parser.hpp"
#include "ast/expression.hpp"
#include <limits>

namespace self {
using namespace detail::parser;

// todo process qualifiers
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
        errReport(notReserved(name), "function name is reserved");
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
        errReport(self::reserved::isEndl(*t++), "invalid expression");
      }
    }
  } catch (std::runtime_error e) {
    err.errors.push_back(self::Error{t.col, t.line, e.what()});
    while (*t != ";")
      ++t;
    goto retry;
  }
}

Module parse(LexedFileRef &in, Context &c) {
  ErrorList e;
  std::vector<const self::StructDef *> list;
  auto parser = GlobalParser(c, e, list);
  auto root = Index(c.root);
  auto ast = parser.process(TokenIt{in}, root);
  return Module(std::move(root), std::move(ast), std::move(e), std::move(list));
}
} // namespace self