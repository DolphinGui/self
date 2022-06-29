#include "builtins.hpp"
#include "container_types.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"
#include <functional>
#include <memory>
#include <sstream>

namespace {
auto is_integer(selflang::token_view t) {
  char *p;
  auto number = strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}
constexpr auto qualifier_guard = [](auto a) {
  return selflang::reserved::is_qualifier(a);
};
constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_keyword(t) &&
         !selflang::reserved::is_grammar(t);
};

struct global_parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_tree &syntax_tree;
  static inline selflang::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  selflang::vector<const selflang::expression *> symbols;
  selflang::expression_ptr current;
  using self = global_parser;
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition, selflang::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }
  selflang::expression_ptr evaluate_tree(selflang::expression_tree &uneval);
  void parse_symbol(auto *base, auto &it, auto &tree) {
    if (auto *maybe = dynamic_cast<selflang::maybe_expression *>(base);
        maybe && !maybe->is_complete()) {
      for (auto symbol : symbols) {
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
    } else if (auto *uneval = dynamic_cast<selflang::expression_tree *>(base);
               uneval) {
      tree.template replace_emplace<selflang::indirector>(
          it, evaluate_tree(*uneval));
    }
  }
  void evaluate() {
    auto t = syntax_tree.rbegin();
    auto &tree = dynamic_cast<selflang::expression_tree &>(*t);
    syntax_tree.replace(t, std::move(*evaluate_tree(tree)));
    return;
  }
  enum struct fsa_states { init, var, fun, expr };
  enum struct var_states { init, named, semicolon, type_annotated, expr };
  enum struct expr_states { init, processing };
  fsa_states fsa_state = fsa_states::init;
  var_states var_state = var_states::init;
  expr_states expr_state = expr_states::init;
  void eat_prev() {
    auto prev = std::move(current);
    auto curr = std::make_unique<selflang::expression_tree>();
    curr->push_back(std::move(dynamic_cast<selflang::var_decl &>(*prev)));
    current = std::move(curr);
  }

  bool expr_parse(selflang::token_view t, bool eat_curr = false) {
    using enum expr_states;
    switch (expr_state) {
    case init:
      if (eat_curr)
        eat_prev();
      expr_state = processing;
      break;
    case processing:
      if (t == selflang::reserved::endl) {
        expr_state = init;
        return true;
      } else {
        dynamic_cast<selflang::expression_tree &>(*current).push_back(
            selflang::maybe_expression(t));
      }
      break;
    }
    return false;
  }

  bool var_parse(selflang::token_view t) {
    using enum var_states;
    using namespace selflang::reserved;
    switch (var_state) {
    case init:
      if (reserved_guard(t)) {
        current = std::make_unique<selflang::var_decl>(t);
        var_state = named;
      } else {
        std::stringstream err;
        err << "token " << t << " is reserved";
        err_assert(false, err.str());
      }
      break;
    case named:
      if (t == ":") {
        var_state = type_annotated;
        break;
      } else if (reserved_guard(t)) {
        var_state = expr;
        [[fallthrough]];
      }
    case expr:
      if (expr_parse(t, true)) {
        var_state = init;
      }
      break;
    case semicolon:
      if (reserved_guard(t)) {
        var_state = type_annotated;
      }
      break;
    case type_annotated:
      if (t == endl) {
        var_state = init;
      }
      break;
    }
    return false;
  }

  void process(selflang::token_view t) {
    switch (fsa_state) {
    case fsa_states::var:
      if (var_parse(t))
        fsa_state = fsa_states::init;
      break;
    case fsa_states::fun:
    // implement later
    case fsa_states::init:
      if (t == selflang::reserved::var_t) {
        fsa_state = fsa_states::var;
        break;
      } else if (reserved_guard(t)) {
        fsa_state = fsa_states::expr;
        [[fallthrough]];
      }
    case fsa_states::expr:
      if (expr_parse(t)) {
        fsa_state = fsa_states::init;
      }
      break;
    }
  }

  global_parser(selflang::expression_tree &syntax_tree)
      : syntax_tree(syntax_tree) {
    types.push_back(&selflang::void_type);
    types.push_back(&selflang::byte_type);
    types.push_back(&selflang::type_var);
    types.push_back(&selflang::int_type);
    types.push_back(&selflang::int_token_t);
    symbols.push_back(&selflang::int_token_assignment);
    symbols.push_back(&selflang::internal_addi);
    symbols.push_back(&selflang::internal_subi);
    symbols.push_back(&selflang::internal_muli);
    symbols.push_back(&selflang::internal_divi);
  }
};
selflang::expression_ptr
global_parser::evaluate_tree(selflang::expression_tree &uneval) {
  auto &tree = dynamic_cast<selflang::expression_tree &>(uneval);
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
    if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it));
        a && !a->get_def().is_member()) {
      auto lhs = it, rhs = it;
      --lhs, ++rhs;
      auto left = tree.pop(lhs), right = tree.pop(rhs);
      remove_indirection(left);
      remove_indirection(right);
      a->add_arg(std::move(left));
      a->add_arg(std::move(right));
    }
  }

  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    if (auto *a = dynamic_cast<selflang::fun_call *>(&(*it));
        a && a->get_def().is_member()) {
      auto lhs = it, rhs = it;
      --lhs, ++rhs;
      auto left = tree.pop(lhs), right = tree.pop(rhs);
      remove_indirection(left);
      remove_indirection(right);
      a->add_arg(std::move(right));
      a->add_arg(std::move(left));
    }
  }
  err_assert(tree.size() == 1, "tree is not fully resolved");
  return tree.pop_front();
}
} // namespace

namespace selflang {
expression_tree parse(token_vec &in) {
  expression_tree syntax_tree;
  return syntax_tree;
}
} // namespace selflang