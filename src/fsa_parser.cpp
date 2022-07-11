#include "builtins.hpp"
#include "container_types.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "syntax_tree.hpp"

#include <functional>
#include <iterator>
#include <memory>
#include <sstream>
#include <stdexcept>

namespace {
auto is_integer(selflang::token_view t) {
  char *p;
  auto number = strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}

auto is_char(selflang::token_view t) {
  if (t.at(0) != '\'')
    return std::pair{false, (unsigned char)'\0'};
  if (t.length() != 3)
    return std::pair{false, (unsigned char)'\0'};
  return std::pair{true, (unsigned char)t.at(1)};
}

constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_keyword(t) &&
         !selflang::reserved::is_grammar(t);
};

struct global_parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_tree syntax_tree;
  std::vector<selflang::expression_tree *> context_stack;
  static inline selflang::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  selflang::vector<const selflang::expression *> symbols;
  std::vector<selflang::expression_ptr> current;
  using self = global_parser;
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition, selflang::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }
  selflang::expression_ptr evaluate_tree(selflang::expression_tree &uneval);
  void parse_symbol(auto &base) {
    if (auto *maybe = dynamic_cast<selflang::maybe_expression *>(base.get());
        maybe && !maybe->is_complete()) {
      auto t = maybe->get_token();
      if (auto [is_int, number] = is_integer(t); is_int) {
        base = std::make_unique<selflang::int_literal>(number);
        return;
      } else if (auto [result, c] = is_char(t); result) {
        base = std::make_unique<selflang::char_literal>(c);
        return;
      }
      for (auto symbol : symbols) {
        if (symbol->getName() == maybe->get_token()) {
          if (typeid(*symbol) == typeid(selflang::operator_def)) {
            base = std::make_unique<selflang::op_call>(
                reinterpret_cast<const selflang::operator_def &>(*symbol));
            return;
          } else if (typeid(*symbol) == typeid(selflang::fun_def)) {
            base = std::make_unique<selflang::fun_call>(
                reinterpret_cast<const selflang::fun_def &>(*symbol));
          }
          err_assert(false, "conflicting symbols");
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
    auto curr = std::move(current.back());
    current.pop_back();
    auto &tree = dynamic_cast<selflang::expression_tree &>(*curr.get());
    current.push_back(evaluate_tree(tree));
  }
  enum struct decl_states { start, init, var, fun, expr };
  enum struct exec_states { start, init, var, expr, ret };
  enum struct var_states { init, named, semicolon, type_annotated, expr };
  enum struct expr_states { init, processing };
  enum struct fun_states {
    init,
    named,
    arg_start,
    arg_named,
    arg_colon,
    arg_finished,
    args_specified,
    forwarded,
    arrow,
    declared,
    parsing
  };
  decl_states fsa_state = decl_states::init;
  var_states var_state = var_states::init;
  expr_states expr_state = expr_states::init;
  fun_states fun_state = fun_states::init;
  exec_states fun_inner_state = exec_states::start;
  using callback = std::function<void()>;

  void eat_prev() {
    auto prev = std::move(current.back());
    current.pop_back();
    auto curr = std::make_unique<selflang::expression_tree>();
    curr->push_back(std::move(prev));
    current.push_back(std::move(curr));
  }

  bool expr_parse(selflang::token_view t, callback start = nullptr,
                  callback insert_call = nullptr) {
    using enum expr_states;
    switch (expr_state) {
    case init:
      if (start)
        start();
      else {
        current.push_back(std::make_unique<selflang::expression_tree>());
      }
      expr_state = processing;
      [[fallthrough]];
    case processing:
      if (t == selflang::reserved::endl) {
        evaluate();
        if (!insert_call) {
          context_stack.back()->emplace_back(std::move(current.back()));
          current.pop_back();
        } else {
          insert_call();
        }
        expr_state = init;
        return true;
      } else {
        auto &curr_expr =
            dynamic_cast<selflang::expression_tree &>(*current.back());
        curr_expr.push_back(std::make_unique<selflang::maybe_expression>(t));
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
        current.push_back(std::make_unique<selflang::var_decl>(t));
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
      if (expr_parse(t, [this] { eat_prev(); })) {
        var_state = init;
        return true;
      }
      break;
    case semicolon:
      if (reserved_guard(t)) {
        for (auto a : types) {
          if (a->getName() == t) {
            reinterpret_cast<selflang::var_decl &>(*current.back()).type.ptr =
                a;
            break;
          }
        }
        var_state = type_annotated;
      }
      break;
    case type_annotated:
      if (t == endl) {
        context_stack.back()->emplace_back(std::move(current.back()));
        current.pop_back();
        var_state = init;
      }
      return true;
    }
    return false;
  }

  bool fun_parse(selflang::token_view t) {
    using enum fun_states;
    selflang::fun_def *fun_ptr;
    if (!current.empty())
      fun_ptr = dynamic_cast<selflang::fun_def *>(current.back().get());
    else
      fun_ptr = nullptr;
    switch (fun_state) {
    case init:
      if (reserved_guard(t)) {
        current.push_back(std::make_unique<selflang::fun_def>(t));
        fun_state = named;
      } else
        err_assert(false, "token is reserved");
      break;
    case named:
      if (t == "(") {
        fun_state = arg_start;
      } else
        err_assert(false, "\"(\" expected");
      break;
    case arg_start:
      if (t == ")") {
        fun_state = args_specified;
      } else if (reserved_guard(t)) {
        fun_ptr->arguments.emplace_back(
            std::make_unique<selflang::var_decl>(t));
        fun_state = arg_named;
      } else
        err_assert(false, "argument or \")\" expected");
      break;
    case arg_named:
      if (t == ":") {
        fun_state = arg_colon;
      } else
        err_assert(false, "\":\" expected");
      break;
    case arg_colon:
      for (auto &type : types) {
        if (type->getName() == t) {
          fun_ptr->arguments.back()->type.ptr = type;
          goto type_found;
        }
      }
      err_assert(false, "no types found");
    type_found:
      fun_state = arg_finished;
      break;
    case arg_finished:
      if (t == ")") {
        fun_state = args_specified;
      } else if (t == ",") {
        fun_state = arg_start;
      } else {
        err_assert(false, "unknown token");
      }
      break;
    case args_specified:
      if (t == "->") {
        fun_state = arrow;
      } else if (t == "{") {
        fun_state = parsing;
      } else {
        err_assert(false, "\"{\" or \"->\" expected.");
      }
      break;
    case arrow:
      for (auto &type : types) {
        if (type->getName() == t) {
          fun_ptr->return_type = type;
          goto return_type_found;
        }
      }
      err_assert(false, "no types found");
    return_type_found:
      fun_state = forwarded;
      break;
    case forwarded:
      fun_ptr->body_defined = false;
      if (t == "{") {
        fun_state = parsing;
      } else if (t == ";") {
        symbols.push_back(current.back().get());
        context_stack.back()->push_back(std::move(current.back()));
        current.pop_back();
        fun_state = init;
        return true;
      } else {
        err_assert(false, "\"{\" or \"->\" expected.");
      }
      break;
    case declared:
      if (t == "{")
        fun_state = parsing;
      else if (t == selflang::reserved::endl) {
        context_stack.back()->push_back(std::move(current.back()));
        current.pop_back();
        fun_state = init;
        return true;
      } else
        err_assert(false, "\"{\" expected.");
      break;
    case parsing: {
      using enum exec_states;
      using namespace selflang::reserved;
      switch (fun_inner_state) {
      case start:
        context_stack.push_back(&fun_ptr->body);
        fun_inner_state = init;
      case init:
        if (t == var_t) {
          fun_inner_state = var;
          break;
        } else if (t == return_t) {
          current.push_back(std::make_unique<selflang::ret>());
          fun_inner_state = ret;
          break;
        } else if (reserved_guard(t)) {
          fun_inner_state = expr;
          [[fallthrough]];
        } else if (t == "}") {
          context_stack.pop_back();
          symbols.push_back(current.back().get());
          context_stack.back()->emplace_back(std::move(current.back()));
          current.pop_back();
          fun_ptr->body_defined = true;
          fun_state = fun_states::init;
          fun_inner_state = start;
          return true;
        } else {
          err_assert(false, "Unexpected token");
        }
      case expr:
        if (expr_parse(t)) {
          fun_inner_state = init;
        }
        break;
      case var:
        if (var_parse(t))
          fun_inner_state = init;
        break;
      case ret:
        const auto insert = [this] {
          auto expr = std::move(current.back());
          current.pop_back();
          dynamic_cast<selflang::ret &>(*current.back()).value =
              std::move(expr);
          context_stack.back()->push_back(std::move(current.back()));
          current.pop_back();
        };
        if (t == endl && expr_state == expr_states::init) {
          context_stack.back()->push_back(std::move(current.back()));
          current.pop_back();
        } else {
          if (expr_parse(t, nullptr, insert)) {
            fun_inner_state = init;
          }
        }
        break;
      }
    } break;
    }
    return false;
  }

  void process(selflang::token_view t) {
    using enum decl_states;
    using namespace selflang::reserved;
    switch (fsa_state) {
    case start:
    case init:
      if (t == var_t) {
        fsa_state = var;
        break;
      } else if (t == fun_t) {
        fsa_state = fun;
        break;
      } else if (reserved_guard(t)) {
        fsa_state = expr;
        [[fallthrough]];
      } else {
        break; /*does absolutely nothing.*/
      }
    case expr:
      if (expr_parse(t)) {
        fsa_state = init;
      }
      break;
    case var:
      if (var_parse(t))
        fsa_state = init;
      break;
    case fun:
      if (fun_parse(t))
        fsa_state = init;
      break;
    }
  }

  global_parser() {
    types.push_back(&selflang::void_type);
    types.push_back(&selflang::byte_type);
    types.push_back(&selflang::type_var);
    types.push_back(&selflang::int_type);
    types.push_back(&selflang::char_type);
    symbols.push_back(&selflang::int_token_assignment);
    symbols.push_back(&selflang::internal_addi);
    symbols.push_back(&selflang::internal_subi);
    symbols.push_back(&selflang::internal_muli);
    symbols.push_back(&selflang::internal_divi);
    context_stack.push_back(&syntax_tree);
  }
};
selflang::expression_ptr
global_parser::evaluate_tree(selflang::expression_tree &tree) {
  for (auto &ptr : tree) {
    parse_symbol(ptr);
  }

  for (auto it = tree.begin(); it != tree.end(); ++it) {
    // if(typeid(*it) == typeid(selflang::fun))
  }

  // left-right associative pass
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *a = dynamic_cast<selflang::op_call *>(it->get());
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
  // right-left associative pass
  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    if (auto *a = dynamic_cast<selflang::op_call *>(it->get());
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
expression_tree parse(token_vec in) {
  global_parser parser;
  std::string debug;
  for (auto t : in) {
    parser.process(t);
    std::stringstream s;
    s << parser.syntax_tree << '\n';
    debug = s.str();
  }
  auto result = std::move(parser.syntax_tree);
  return result;
}
} // namespace selflang