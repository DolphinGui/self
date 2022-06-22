#include "fsa_parser.hpp"
#include "builtins.hpp"
#include "container_types.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <boost/sml.hpp>
#include <cctype>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <utility>
#include <variant>

namespace {
using namespace boost::sml::literals;
constexpr auto qualifier_guard = [](auto a) {
  return selflang::reserved::is_qualifier(a);
};
constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_reserved(t);
};

struct expression_parser {
  using self = expression_parser;
  auto operator()() const noexcept {
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table("init"_s + event<token_view> = "state"_s);
  }
};
struct eof {};

auto is_integer(selflang::token_view t) {
  char *p;
  auto number = strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}
struct parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_list &syntax_tree;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  selflang::vector<const selflang::expression *> symbols;
  selflang::vector<selflang::expression_tree *> expressions;
  selflang::var_decl *current;
  using self = parser;
  void add_var(selflang::token_view t) {
    auto result = std::make_unique<selflang::var_decl>(t);
    current = result.get();
    syntax_tree.emplace_back(std::move(result));
  }

  void expr_add_token(selflang::token_view t) {
    auto &tree = dynamic_cast<selflang::expression_tree &>(*expressions.back());
    tree.push_back(selflang::maybe_expression(t));
  }

  void add_expr(selflang::token_view t) {
    auto expr = (std::make_unique<selflang::expression_tree>());
    // this just returns current, but current has more
    bool this_should_be_true = (syntax_tree.back().release() == current);
    if (!this_should_be_true)
      throw std::runtime_error("corrupted expression tree.");
    expr->push_back(std::move(*current));
    delete current;
    syntax_tree.pop_back();
    expressions.push_back(expr.get());
    syntax_tree.emplace_back(std::move(expr));
    expr_add_token(t);
  }

  // I normally dislike doing this, but I can't seem to figure out why
  // defining a constructor breaks this.
  void setup() {
    types.push_back(&selflang::void_type);
    types.push_back(&selflang::byte_type);
    types.push_back(&selflang::type_var);
    types.push_back(&selflang::int_token_t);
    symbols.push_back(&selflang::int_token_assignment);
    symbols.push_back(&selflang::internal_addi);
    symbols.push_back(&selflang::internal_subi);
    symbols.push_back(&selflang::internal_muli);
    symbols.push_back(&selflang::internal_divi);
  }

  std::optional<std::variant<selflang::int_literal, selflang::fun_call>>
  parse_symbol(auto *maybe, auto symbol) {
    if (!maybe->is_complete()) {
      if (symbol->getName() == maybe->get_token()) {
        if (typeid(*symbol) == typeid(selflang::operator_def)) {
          return selflang::fun_call(
              dynamic_cast<const selflang::operator_def &>(*symbol));
        }
      } else if (auto [is_int, number] = is_integer(maybe->get_token());
                 is_int) {
        return selflang::int_literal(number);
      }
    }
    return std::nullopt;
  }
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition) {
    if (!condition)
      throw std::runtime_error("assert failed.");
  }
  auto make_tree(auto uneval) {
    auto &tree = dynamic_cast<selflang::expression_tree &>(*uneval);
    auto contents = tree.dump_contents();
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      auto *maybe = dynamic_cast<selflang::maybe_expression *>(&(*it));
      if (maybe)
        for (auto symbol : symbols) {
          auto result = parse_symbol(maybe, symbol);
          if (result) {
            if (result->index() == 0) {
              using T = std::decay_t<decltype(std::get<0>(result.value()))>;
              tree.replace_emplace<T>(it,
                                      std::move(std::get<0>(result.value())));
            } else if (result->index() == 1) {
              using T = std::decay_t<decltype(std::get<1>(result.value()))>;
              tree.replace_emplace<T>(it,
                                      std::move(std::get<1>(result.value())));
            } else {
              throw std::runtime_error("I don't know how this happened.");
            }
            break;
          }
        }
    }

    for (auto it = tree.begin(); it != tree.end(); ++it) {
      contents = tree.dump_contents();
      if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it)); a) {
        if (!a->get_def().is_member()) {
          auto lhs = it;
          auto rhs = it;
          --lhs;
          ++rhs;
          a->add_arg(tree.pop(lhs));
          a->add_arg(tree.pop(rhs));
        }
      }
    }

    for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
      if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it)); a) {
        if (a->get_def().is_member()) {
          auto lhs = it;
          auto rhs = it;
          ++lhs;
          --rhs;
          a->add_arg(tree.pop(rhs));
          a->add_arg(tree.pop(lhs));
        }
      }
    }
  }

  void eval_expressions() {
    for (auto uneval : expressions) {
      make_tree(uneval);
    }
  }

  void add_type(selflang::token_view t) {
    for (auto &type : types) {
      if (type->getName() == t) {
        // this assumes the variable created is on the back
        current->var_type = type;
        return;
      }
    }
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
        "var decl"_s + event<token_view>[reserved_guard] / &self::add_var =
            "var named"_s,
        "var named"_s + event<token_view>[([](auto t) { return t == ":"; })] =
            "type annotation"_s,
        "type annotation"_s + event<token_view> / &self::add_type = "end var"_s,
        "end var"_s + event<token_view>[([](auto t) { return t == ";"; })] /
                          [] { std::cout << "endl\n"; } = "init"_s,
        "var named"_s + event<token_view>[reserved_guard] / &self::add_expr =
            "var expression"_s,
        "var expression"_s + event<token_view>[reserved_guard] /
                                 &self::expr_add_token = "var expression"_s,
        "var expression"_s +
            event<token_view>[([](auto t) { return t == ";"; })] /
                [] { std::cout << "endl\n"; } = "init"_s,
        "init"_s + event<eof> / &self::eval_expressions);
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
  fsa.process_event(eof{});
  return syntax_tree;
}
} // namespace selflang