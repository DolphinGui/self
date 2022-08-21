#include "parser.hpp"

#include "ffi_parse.hpp"

#include <fstream>
namespace {
using namespace self::detail::parser;
auto fileOpen(self::Pos loc, std::string path) {
  auto file = std::fstream(path);
  errReport(file.good(), loc, "file has failed to open");
  std::stringstream result;
  result << file.rdbuf();
  return result.str();
}

auto parseStrLit(TokenIt &t, std::string err) {
  auto a = isStr(*t);
  errReport(!a->empty(), t.coord(), std::move(err));
  std::string unconvert;
  unconvert.reserve(a->size());
  std::for_each(a->begin(), a->end(),
                [&](unsigned char c) { unconvert.push_back(c); });
  return unconvert;
}
} // namespace
namespace self::detail::parser {

void GlobalParser::processImport(TokenIt &t, self::ExprTree &syntax_tree,
                                 self::Index &global) {
  auto path = parseStrLit(t, "Import path must be a string literal");
  auto file = fileOpen(t.coord(), path);
  auto tokens = self::detail::parseToken(self::detail::preprocess(file));
  process(TokenIt{tokens}, syntax_tree, global);
  ++t;
}

void GlobalParser::processExtern(TokenIt &t, self::ExprTree &syntax_tree) {
  auto spec = parseStrLit(t, "extern specification must be a string literal");
  if (spec == "C") {
    ++t;
    errReport(*t == self::reserved::import_t, t,
              "expected import after extern specification");
    auto path = parseStrLit(t, "Import path must be a string literal");
    ++t;
    self::parseFFI(syntax_tree, c.root, c, path, "-O2");
  } else {
    errReport(false, t.coord(), "unknown extern specification");
  }
}
} // namespace self::detail::parser