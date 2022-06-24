#include "fsa_parser.hpp"
#include "builtins.hpp"
#include "container_types.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <boost/sml.hpp>
#include <iostream>
#include <queue>

namespace {
auto is_integer(selflang::token_view t) {
  char *p;
  auto number = strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}
using namespace boost::sml::literals;
constexpr auto qualifier_guard = [](auto a) {
  return selflang::reserved::is_qualifier(a);
};
constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_keyword(t) &&
         !selflang::reserved::is_grammar(t);
};
constexpr auto endl = [](auto t) { return t == ";"; };
constexpr auto open_paren = [](auto t) { return t == "("; };
constexpr auto close_paren = [](auto t) { return t == ")"; };
constexpr auto arrow = [](auto t) { return t == "->"; };
constexpr auto open_bracket = [](auto t) { return t == "{"; };
constexpr auto close_bracket = [](auto t) { return t == "}"; };
constexpr auto comma = [](auto t) { return t == ","; };
constexpr auto var = [](auto t) { return t == selflang::reserved::var_t; };
constexpr auto fun = [](auto t) { return t == selflang::reserved::fun_t; };
constexpr static auto token_event = boost::sml::event<selflang::token_view>;
struct context {
  selflang::expression_list &syntax_tree;
  selflang::type_list &types;
  selflang::vector<const selflang::expression *> &symbols;
};
struct expression_parser {
  context c;
  selflang::vector<selflang::unevaluated_expression *> stack;
  selflang::expression **current;
  selflang::token_view &passthrough;
  using eself = expression_parser;
  void expr_add_token(selflang::token_view t) {
    auto &tree =
        dynamic_cast<selflang::unevaluated_expression &>(*c.syntax_tree.back());
    if (t == "(") {
      stack.back()->push_back(selflang::unevaluated_expression{});
      stack.push_back(reinterpret_cast<selflang::unevaluated_expression *>(
          &stack.back()->back()));
    } else if (t == ")") {
      stack.pop_back();
    } else {
      stack.back()->push_back(selflang::maybe_expression(t));
    }
  }

  // todo: add proper compile error reporting later.
  auto err_assert(bool condition) {
    if (!condition)
      throw std::runtime_error("assert failed.");
  }

  auto err_assert(bool condition, const char *message) {
    if (!condition)
      throw std::runtime_error(message);
  }

  void add_expr(selflang::token_view t) {
    auto expr = std::make_unique<selflang::unevaluated_expression>();
    // this just returns current, but current has more
    if (*current) {
      err_assert(c.syntax_tree.back().release() == *current,
                 "syntax tree is corrupted");
      expr->push_back(selflang::indirector(*current));
      c.syntax_tree.pop_back();
      stack.push_back(expr.get());
    }
    c.syntax_tree.emplace_back(std::move(expr));
    expr_add_token(passthrough);
    expr_add_token(t);
  }

  selflang::expression_ptr
  evaluate_tree(selflang::unevaluated_expression &uneval);

  void parse_symbol(auto *base, auto &it, auto &tree) {
    if (auto *maybe = dynamic_cast<selflang::maybe_expression *>(base); maybe) {
      if (maybe && !maybe->is_complete()) {
        for (auto symbol : c.symbols) {
          if (symbol->getName() == maybe->get_token()) {
            if (typeid(*symbol) == typeid(selflang::operator_def)) {
              tree.template replace_emplace<selflang::fun_call>(
                  it, dynamic_cast<const selflang::operator_def &>(*symbol));
            }
            break;
          } else if (auto [is_int, number] = is_integer(maybe->get_token());
                     is_int) {
            tree.template replace_emplace<selflang::int_literal>(it, number);
            break;
          }
        }
      }
    } else if (auto *uneval =
                   dynamic_cast<selflang::unevaluated_expression *>(base);
               uneval) {
      tree.template replace_emplace<selflang::indirector>(
          it, evaluate_tree(*uneval));
    }
  }

  void evaluate() {
    auto &t = c.syntax_tree.back();
    if (auto *tree = dynamic_cast<selflang::unevaluated_expression *>(t.get());
        tree) {
      t.reset(evaluate_tree(*tree).release());
      return;
    }
    err_assert(false, "tree corrupted somehow.");
  }

  auto operator()() const noexcept {
    using self = expression_parser;
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table(
        *"init"_s + token_event[reserved_guard && !endl] / &self::add_expr =
            "start"_s,
        "start"_s + token_event[reserved_guard && !endl] /
                        &self::expr_add_token = "start"_s,
        "start"_s + token_event[endl || comma] / &self::evaluate = X);
  }
};
struct var_parser {
  context c;
  selflang::expression *current;
  selflang::token_view current_token;

  using self = expression_parser;

  void pass_token(selflang::token_view t) { current_token = t; }

  void add_var(selflang::token_view t) {
    auto result = std::make_unique<selflang::var_decl>(t);
    current = result.get();
    c.syntax_tree.emplace_back(std::move(result));
  }

  void add_type(selflang::token_view t) {
    for (auto &type : c.types) {
      if (type->getName() == t) {
        // this assumes the variable created is on the back
        reinterpret_cast<selflang::var_decl *>(current)->var_type = type;
        return;
      }
    }
  }

  auto operator()() const noexcept {
    using self = var_parser;
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table(
        *"var decl"_s + token_event[reserved_guard] / &self::add_var =
            "var named"_s,
        "var named"_s + token_event[([](auto t) { return t == ":"; })] =
            "type annotation"_s,
        "type annotation"_s + token_event / &self::add_type = "end var"_s,
        "end var"_s + token_event[endl] = X,
        "var named"_s + token_event[reserved_guard && !endl] /
                            &self::pass_token = state<expression_parser>,
        state<expression_parser> = X);
  }
};
selflang::expression_ptr
expression_parser::evaluate_tree(selflang::unevaluated_expression &uneval) {
  auto &tree = dynamic_cast<selflang::unevaluated_expression &>(uneval);
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    parse_symbol(&(*it), it, tree);
  }
  const auto remove_indirection = [](selflang::expression_ptr &ptr) {
    if (auto *indirector = dynamic_cast<selflang::indirector *>(ptr.get());
        indirector) {
      ptr.reset(indirector->inner.release());
    }
  };

  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it)); a) {
      if (!a->get_def().is_member()) {
        auto lhs = it, rhs = it;
        --lhs, ++rhs;
        auto left = tree.pop(lhs), right = tree.pop(rhs);
        remove_indirection(left);
        remove_indirection(right);
        a->add_arg(std::move(left));
        a->add_arg(std::move(right));
      }
    }
  }

  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it)); a) {
      if (a->get_def().is_member()) {
        auto lhs = it, rhs = it;
        --lhs, ++rhs;
        auto left = tree.pop(lhs), right = tree.pop(rhs);
        remove_indirection(left);
        remove_indirection(right);
        a->add_arg(std::move(right));
        a->add_arg(std::move(left));
      }
    }
  }
  err_assert(tree.size() == 1, "tree is not fully resolved");
  return tree.pop_front();
}
struct fun_parser {
  context c;
  auto operator()() const noexcept {
    using self = fun_parser;
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table(
        *"fun decl"_s + token_event[reserved_guard] = "fun named"_s,
        "fun named"_s + token_event[open_paren] = "args"_s,
        "args"_s + token_event[reserved_guard] = "arg named"_s,
        "arg named"_s + token_event[([](auto t) { return t == ":"; })] =
            "arg type"_s,
        "arg type"_s + token_event[reserved_guard] = "arg typed"_s,
        "arg typed"_s + token_event[comma] = "args"_s,
        "args"_s + token_event[close_paren] = "args defined"_s,
        "args defined"_s + token_event[arrow] = "ret type"_s,
        "ret type"_s + token_event[reserved_guard] = "pre declared"_s,
        "pre declared"_s + token_event[open_bracket] = "body"_s,
        "args defined"_s + token_event[open_bracket] = "body"_s,
        "body"_s + token_event[reserved_guard] = state<expression_parser>,
        state<expression_parser> = "body"_s,
        "body"_s + token_event[var] = state<var_parser>,
        state<var_parser> = "body"_s,
        "body"_s + token_event[close_bracket] = X);
  }
};
struct global_parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_list &syntax_tree;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  selflang::vector<const selflang::expression *> symbols;
  using self = global_parser;
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition) {
    if (!condition)
      throw std::runtime_error("assert failed.");
  }

  auto err_assert(bool condition, const char *message) {
    if (!condition)
      throw std::runtime_error(message);
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

  auto operator()() const noexcept {
    using namespace boost::sml;
    using namespace selflang;
    using namespace selflang::reserved;
    return make_transition_table(
        *"start"_s / &self::setup = "init"_s,
        "init"_s + token_event[qualifier_guard] = "init"_s,
        "init"_s + token_event[var] = state<var_parser>,
        state<var_parser> = "init"_s,
        "init"_s + token_event[fun] = state<fun_parser>,
        state<fun_parser> = "init"_s, "init"_s + token_event[endl]);
  }
};
} // namespace

namespace selflang {
expression_list parse(token_vec &in) {
  expression_list syntax_tree;
  auto dep = global_parser{syntax_tree};
  auto c = context{
      .syntax_tree = syntax_tree, .types = dep.types, .symbols = dep.symbols};
  auto vardeps = var_parser{.c = c};
  auto exprdeps = expression_parser{.c = c,
                                    .current = &(vardeps.current),
                                    .passthrough = vardeps.current_token};
  auto fundeps = fun_parser{.c = c};
  auto fsa = boost::sml::sm<global_parser>(dep, vardeps, fundeps, exprdeps);
  for (token_view token : in) {
    fsa.process_event(token);
  }
  return syntax_tree;
}
} // namespace selflang