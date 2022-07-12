#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <tuple>

#include "ast/control.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/unevaluated_expression.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include "lexer.hpp"
#include "literals.hpp"

namespace {
auto is_integer(selflang::token_view t) {
  char *p;
  auto number = std::strtol(t.data(), &p, 10);
  return std::pair{*p == 0, number};
}

auto is_char(selflang::token_view t) {
  if (t.front() != '\'')
    return std::pair{false, (unsigned char)'\0'};
  if (t.length() != 3)
    return std::pair{false, (unsigned char)'\0'};
  return std::pair{true, (unsigned char)t.at(1)};
}

std::pair<bool, std::string> is_str(selflang::token_view t) {
  if (t.front() != '\'' && t.front() != '\"')
    return {false, ""};
  if (!t.ends_with('\'') && !t.ends_with('\"'))
    return {false, ""};
  auto literal = std::string(t);
  for (auto c = literal.begin(); c != literal.end(); ++c) {
    if (*c == '\\') {
      auto mark = c;
      ++c;
      if (std::isdigit(*c)) {
        char *end;
        auto value = std::strtol(&*c, &end, 10);
        if (value > std::numeric_limits<unsigned char>::max()) {
          throw std::runtime_error("escaped char value is too big.");
        }
        // this is a stupid workaround but I guess it'll work
        auto distance = std::distance(&*c, end);
        literal.erase(c, c + 1 + distance);
        *mark = value;
      } else if (*c == 'n') {
        *mark = '\n';
        literal.erase(c);
      }
    }
  }
  return {true, std::string(t.substr(1, t.length() - 2))};
}

constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_keyword(t) &&
         !selflang::reserved::is_grammar(t);
};

struct global_parser {
  // syntax tree must be first because of initialization rules
  selflang::expression_tree syntax_tree;
  std::vector<selflang::expression_tree *> context_stack;
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  std::vector<const selflang::expression *> symbols;
  std::vector<selflang::expression_ptr> current;
  using self = global_parser;
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }
  selflang::expression_ptr evaluate_tree(selflang::expression_tree &uneval);
  void parse_symbol(auto &base) {
    if (auto *maybe =
            dynamic_cast<selflang::unevaluated_expression *>(base.get());
        maybe && !maybe->is_complete()) {
      auto t = maybe->get_token();
      if (auto [is_int, number] = is_integer(t); is_int) {
        base = std::make_unique<selflang::int_literal>(number);
        return;
      } else if (auto [result, c] = is_char(t); result) {
        base = std::make_unique<selflang::char_literal>(c);
        return;
      } else if (auto [result, str] = is_str(t); result) {
        std::vector<unsigned char> value;
        value.reserve(str.size());
        std::copy(str.begin(), str.end(), std::back_inserter(value));
        base = std::make_unique<selflang::string_literal>(value);
        return;
      }
      for (auto symbol : symbols) {
        if (symbol->getName() == maybe->get_token()) {
          if (auto &hash = typeid(*symbol);
              hash == typeid(selflang::operator_def)) {
            base = std::make_unique<selflang::op_call>(
                reinterpret_cast<const selflang::operator_def &>(*symbol));
            return;
          } else if (hash == typeid(selflang::fun_def)) {
            base = std::make_unique<selflang::fun_call>(
                reinterpret_cast<const selflang::fun_def_base &>(*symbol));
            return;
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
        curr_expr.push_back(
            std::make_unique<selflang::unevaluated_expression>(t));
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

  // ok not sure why the end check doesn't work but I have to check
  // if the tree is size 1
  for (auto it = tree.begin(); it != tree.end() && tree.size() != 1; ++it) {
    selflang::expression_tree arg;
    if (auto *fun = reinterpret_cast<selflang::fun_call *>(it->get())) {
      ++it;
      if (auto *open_paren =
              dynamic_cast<selflang::unevaluated_expression *>(it->get())) {
        if (open_paren->get_token() == "(") {
          it = tree.erase(it);
          while (true) {
            auto *token_maybe =
                dynamic_cast<selflang::unevaluated_expression *>(it->get());
            if (token_maybe) {
              if (token_maybe->get_token() == ")") {
                if (arg.size() > 0)
                  fun->args.emplace_back(evaluate_tree(arg));
                it = --tree.erase(it);
                break;
              } else if (token_maybe->get_token() == ",") {
                fun->args.emplace_back(evaluate_tree(arg));
                it = --tree.erase(it);
              }
            } else {
              arg.push_back(std::move(*it));
              it = --tree.erase(it);
            }
            ++it;
          }
        }
      }
    }
  }

  // left-right associative pass
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *op = dynamic_cast<selflang::op_call *>(it->get());
        op && !op->get_def().is_member()) {
      auto lhs = it, rhs = it;
      --lhs, ++rhs;
      auto left = selflang::expression_ptr(lhs->release()),
           right = selflang::expression_ptr(rhs->release());
      tree.erase(rhs), it = tree.erase(lhs);
      op->add_arg(std::move(left));
      op->add_arg(std::move(right));
    }
  }
  // right-left associative pass
  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    if (auto *op = dynamic_cast<selflang::op_call *>(it->get());
        op && op->get_def().is_member()) {
      auto lhs = it, rhs = it;
      ++lhs, --rhs;
      auto left = selflang::expression_ptr(lhs->release()),
           right = selflang::expression_ptr(rhs->release());
      op->add_arg(std::move(left));
      op->add_arg(std::move(right));
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
  for (auto t : in) {
    parser.process(t);
  }
  auto result = std::move(parser.syntax_tree);
  return result;
}
} // namespace selflang