#include "builtins.hpp"
#include "container_types.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"

#include <iterator>
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
  selflang::expression_tree syntax_tree;
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
  void parse_symbol(auto &base, selflang::expression_tree &tree) {
    if (auto *maybe = dynamic_cast<selflang::maybe_expression *>(base.get());
        maybe && !maybe->is_complete()) {
      if (auto [is_int, number] = is_integer(maybe->get_token()); is_int) {
        base = std::make_unique<selflang::int_literal>(number);
        return;
      }
      for (auto symbol : symbols) {
        if (symbol->getName() == maybe->get_token() &&
            typeid(*symbol) == typeid(selflang::operator_def)) {
          base = std::move(std::make_unique<selflang::fun_call>(
              reinterpret_cast<const selflang::operator_def &>(*symbol)));
          return;
        }
      }
    } else if (auto *uneval =
                   dynamic_cast<selflang::expression_tree *>(base.get());
               uneval) {
      base = evaluate_tree(*uneval);
      return;
    }
  }
  void evaluate() {
    auto curr = std::move(current);
    auto &tree = dynamic_cast<selflang::expression_tree &>(*curr.get());
    current = evaluate_tree(tree);
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
    curr->push_back(std::move(prev));
    current = std::move(curr);
  }

  bool expr_parse(selflang::token_view t, bool eat_curr = false) {
    using enum expr_states;
    auto *a = dynamic_cast<selflang::expression_tree *>(current.get());
    switch (expr_state) {
    case init:
      if (eat_curr)
        eat_prev();
      else
        current = std::make_unique<selflang::expression_tree>();
      expr_state = processing;
      [[fallthrough]];
    case processing:
      if (t == selflang::reserved::endl) {
        evaluate();
        syntax_tree.emplace_back(std::move(current));
        expr_state = init;
        return true;
      } else {
        dynamic_cast<selflang::expression_tree &>(*current).push_back(
            std::make_unique<selflang::maybe_expression>(t));
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
        var_state = semicolon;
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
        for (auto a : types) {
          if (a->getName() == t) {
            reinterpret_cast<selflang::var_decl &>(*current).var_type = a;
            break;
          }
        }
        var_state = type_annotated;
      }
      break;
    case type_annotated:
      if (t == endl) {
        syntax_tree.emplace_back(std::move(current));
        var_state = init;
      }
      return true;
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

  global_parser() {
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
global_parser::evaluate_tree(selflang::expression_tree &tree) {
  for (auto &ptr : tree) {
    parse_symbol(ptr, tree);
  }

  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *a = dynamic_cast<selflang::fun_call *>(it->get());
        a && !a->get_def().is_member()) {
      auto lhs = it, rhs = it;
      --lhs, ++rhs;
      auto left = selflang::expression_ptr(lhs->release()),
           right = selflang::expression_ptr(rhs->release());
      tree.erase(rhs), it = tree.erase(lhs);
      a->add_arg(std::move(left));
      a->add_arg(std::move(right));
    }
  }

  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    if (auto *a = dynamic_cast<selflang::fun_call *>(it->get());
        a && a->get_def().is_member()) {
      auto lhs = it, rhs = it;
      ++lhs, --rhs;
      auto left = selflang::expression_ptr(lhs->release()),
           right = selflang::expression_ptr(rhs->release());
      a->add_arg(std::move(left));
      a->add_arg(std::move(right));
      tree.erase(rhs.base());
      it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
    }
  }

  err_assert(tree.size() == 1, "tree is not fully resolved");
  auto last = selflang::expression_ptr(tree.front().release());
  tree.pop_back();
  return last;
}
} // namespace

namespace selflang {
expression_tree parse(token_vec &in) {
  global_parser parser;
  for (auto t : in) {
    parser.process(t);
  }
  auto result = std::move(parser.syntax_tree);
  return result;
}
} // namespace selflang