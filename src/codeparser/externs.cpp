#include "parser.hpp"

namespace {
using namespace self::detail::parser;
auto fileOpen(GlobalParser &g, std::string_view p) {
  auto path = std::string(p);
  auto file = std::fstream(path);
  g.errReport(file.good(), "file has failed to open");
  std::stringstream result;
  result << file.rdbuf();
  return result.str();
}

auto parseStrLit(GlobalParser &g, TokenIt &t, std::string_view err) {
  auto a = isStr(*t);
  g.errReport(!a->empty(), err);
  std::string unconvert;
  unconvert.reserve(a->size());
  std::for_each(a->begin(), a->end(),
                [&](unsigned char c) { unconvert.push_back(c); });
  return unconvert;
}
} // namespace

namespace self::detail::parser {
void GlobalParser::processExtern(TokenIt &t, self::ExprTree &syntax_tree) {
  auto spec =
      parseStrLit(*this, t, "extern specification must be a string literal");
  if (spec == "C") {
    ++t;
    errReport(*t++ == self::reserved::import_t,
              "expected import after extern specification");
    auto path = parseStrLit(*this, t, "Import path must be a string literal");
    ++t;
    self::parseFFI(syntax_tree, c.root, c, path, "-O2");
  } else {
    errReport(false, "unknown extern specification");
  }
}

void GlobalParser::processImport(TokenIt &t, self::ExprTree &syntax_tree,
                                 self::Index &global) {
  auto path = parseStrLit(*this, t, "Import path must be a string literal");
  ++t;
  auto file = fileOpen(*this, path);
  auto tokens = self::detail::parseToken(self::detail::preprocess(file));
  process(TokenIt{tokens}, syntax_tree, global);
}
} // namespace self::detail::parser