#include "fsa_parser.hpp"
#include "builtins.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <boost/sml.hpp>
#include <iostream>
#include <memory>

namespace {
using namespace boost::sml::literals;
constexpr auto qualifier_guard = [](auto a) {
  return selflang::reserved::is_qualifier(a);
};
constexpr auto reserved_guard = [](auto t) {
  return selflang::reserved::is_reserved(t);
};
struct parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_list &syntax_tree;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  selflang::vector<const selflang::expression *> symbols;
  selflang::var_decl *current;
  using self = parser;
  void add_var(selflang::token_view t) {
    auto result = std::make_unique<selflang::var_decl>(t);
    current = result.get();
    syntax_tree.emplace_back(std::move(result));
  }
  void add_expr(selflang::token_view t) {}
  // I normally dislike doing this, but I can't seem to figure out why
  // defining a constructor breaks this.
  void setup() {
    types.push_back(&selflang::void_type);
    types.push_back(&selflang::byte_type);
    types.push_back(&selflang::typename_type);
    types.push_back(&selflang::int_token_t);
    symbols.push_back(&selflang::int_token_assignment);
    symbols.push_back(&selflang::internal_addi);
  }
  bool type_guard(selflang::token_view t) {
    for (auto &type : types) {
      if (type->getName() == t) {
        // this assumes the variable created is on the back
        current->var_type = type;
        return true;
      }
    }
    return false;
  }
  auto operator()() const noexcept {
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table(
        *"start"_s / &self::setup = "init"_s,
        "init"_s + event<token_view>[qualifier_guard] = "init"_s,
        "init"_s + event<token_view>[([](auto a) { return a == var_t; })] =
            "var decl"_s,
        "var decl"_s + event<token_view>[!reserved_guard] / &self::add_var =
            "var named"_s,
        "var named"_s + event<token_view>[([](auto t) { return t == ":"; })] =
            "type annotation"_s,
        "var named"_s + event<token_view>[reserved_guard] = "var expression"_s,
        "var expression"_s + event<token_view>[reserved_guard] =
            "var expression"_s,
        "type annotation"_s + event<token_view>[&self::type_guard] = "end"_s,
        "end"_s + event<token_view>[([](auto t) { return t == ";"; })] /
                      [] { std::cout << "endl\n"; } = "init"_s);
  }
};

} // namespace

namespace selflang {
expression_list parse(token_vec &in) {
  expression_list syntax_tree;
  auto dep = parser{syntax_tree};
  auto fsa = boost::sml::sm<parser>(dep);
  for (token_view token : in) {
    fsa.process_event(token);
  }
  return syntax_tree;
}
} // namespace selflang