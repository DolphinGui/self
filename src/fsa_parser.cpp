#include "fsa_parser.hpp"
#include <boost/sml.hpp>

namespace {
using namespace boost::sml::literals;
auto init = *"start"_s;
struct qualifier: public cplang::string_view{};
constexpr auto qualifier_event = boost::sml::event<qualifier>;
struct var_literal{};
constexpr auto var_literal_event = boost::sml::event<var_literal>;
struct struct_literal{};
constexpr auto struct_literal_event = boost::sml::event<struct_literal>;
struct token: public cplang::string_view{};
constexpr auto token_event = boost::sml::event<token>;

struct parser {
  using self = parser;
  auto operator()() const noexcept {
    using namespace boost::sml;
    // clang-format off
    return make_transition_table(
      init + qualifier_event = init
    );
    // clang-format on
  }
};
} // namespace

namespace cplang {
expression_list parse(statement_vec &in) {
  expression_list syntax_tree;
  type_list types;
  symbol_list symbols;
  types.emplace_back(
      const_cast<type_ref>(static_cast<const type *>(&void_type)));
  types.emplace_back(
      const_cast<type_ref>(static_cast<const type *>(&byte_type)));
  
}
} // namespace cplang