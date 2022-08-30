#include "parser.hpp"

namespace {
using namespace self::detail::parser;
auto fileOpen(self::Pos loc, std::string_view p) {
  auto path = std::string(p);
  auto file = std::fstream(path);
  errReport(file.good(), loc, "file has failed to open");
  std::stringstream result;
  result << file.rdbuf();
  return result.str();
}

auto parseStrLit(self::Pos loc, TokenIt &t, std::string err) {
  auto a = isStr(*t);
  errReport(!a->empty(), loc, err);
  std::string unconvert;
  unconvert.reserve(a->size());
  std::for_each(a->begin(), a->end(),
                [&](unsigned char c) { unconvert.push_back(c); });
  return unconvert;
}
} // namespace

namespace self::detail::parser {
void GlobalParser::processExtern(TokenIt &t, self::ExprTree &syntax_tree) {
  auto spec = parseStrLit(t.coord(), t,
                          "extern specification must be a string literal");
  if (spec == "C") {
    ++t;
    errReport(*t == self::reserved::import_t, t.coord(),
              "expected import after extern specification");
    ++t;
    auto path =
        parseStrLit(t.coord(), t, "Import path must be a string literal");
    ++t;
    self::parseFFI(syntax_tree, c.root, c, path, "-O2");
  } else {
    errReport(false, t.coord(), "unknown extern specification");
  }
}

void GlobalParser::processImport(TokenIt &t, self::ExprTree &syntax_tree,
                                 self::Index &global) {
  auto path = parseStrLit(t.coord(), t, "Import path must be a string literal");
  ++t;
  auto file = fileOpen(t.coord(), path);
  auto tokens = self::detail::parseToken(self::detail::preprocess(file));
  process(TokenIt{tokens}, syntax_tree, global);
}
} // namespace self::detail::parser