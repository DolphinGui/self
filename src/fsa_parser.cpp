#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <vector>

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/struct_def.hpp"
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
  return {true, std::string(literal.substr(1, literal.length() - 2))};
}

constexpr auto reserved_guard = [](auto t) {
  return !selflang::reserved::is_keyword(t) &&
         !selflang::reserved::is_grammar(t);
};
template <typename T> struct token_it_t {
  T &where;
  size_t pos = 0;
  selflang::token_view operator*() { return where.at(pos); }
  token_it_t &operator++() {
    if (pos >= where.size()) {
      throw std::runtime_error("out of bounds");
    }
    ++pos;
    return *this;
  }
  token_it_t operator++(int) {
    token_it_t tmp = *this;
    this->operator++();
    return tmp;
  }
};
using token_it = token_it_t<std::vector<selflang::token>>;

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
                reinterpret_cast<const selflang::fun_def &>(*symbol));
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
  enum struct decl_states { init, var, fun, expr };
  enum struct exec_states { init, start, var, expr, ret };
  enum struct var_states { init, named, semicolon, type_annotated, expr };
  enum struct struct_states { init, start, var, fun, opaque, sized };
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
  fun_states fun_state = fun_states::init;
  exec_states fun_inner_state = exec_states::start;
  struct_states struct_state = struct_states::init;
  using callback = std::function<void(selflang::expression_tree &)>;

  selflang::expression_ptr expr_parse(token_it& t, callback start = nullptr) {
    auto tree = std::make_unique<selflang::expression_tree>();
    if (start) {
      start(*tree);
    }
    while (*t != selflang::reserved::endl) {
      tree->push_back(std::make_unique<selflang::unevaluated_expression>(*t++));
    }
    auto result = evaluate_tree(*tree);
    return result;
  }

  selflang::expression_ptr var_parse(token_it& t) {
    using enum var_states;
    using namespace selflang::reserved;
    const auto guard = [this](auto t) {
      if (!reserved_guard(t)) {
        std::stringstream err;
        err << "token " << t << " is reserved";
        err_assert(false, err.str());
      }
    };
    guard(*t);
    auto curr = std::make_unique<selflang::var_decl>(*t++);
    if (*t == ":") {
      ++t;
      // TODO: deal with multiple candidates
      auto candidates = types.find(*t++);
      err_assert(candidates != types.end(), "type not found");
      curr->type.ptr = candidates->second;
      err_assert(*t == endl, "end of line expected");
      return curr;
    } else {
      err_assert(reserved_guard(*t),
                 "non-reserved token expected in expression");
      return expr_parse(t, [&](selflang::expression_tree &tree) {
        tree.push_back(std::move(curr));
      });
    }
  }

  selflang::expression_ptr fun_parse(token_it& t) {
    using enum fun_states;
    err_assert(reserved_guard(*t),
               "reserved token cannot be used as function name");
    auto curr = std::make_unique<selflang::fun_def>(*t++);
    err_assert(*t++ == "(", "\"(\" expected");
    fun_state = arg_start;
    while (*t != ")") {
      err_assert(reserved_guard(*t),
                 "reserved token cannot be used as parameter name");
      curr->arguments.emplace_back(std::make_unique<selflang::var_decl>(*t++));
      err_assert(*t++ == ":", "\":\" expected here");
      // todo make this an expression instead of hardcoded
      auto candidates = types.find(*t);
      err_assert(candidates != types.end(), "no types found");
      curr->arguments.back()->type.ptr = candidates->second;
      err_assert(*t == ")" || *t == ",", "\")\" or \",\" expected");
    }
    if (*t == "->") {
      ++t;
      auto candidates = types.find(*t++);
      err_assert(candidates != types.end(), "type not found");
      curr->return_type = candidates->second;
      curr->body_defined = false;
    }
    if (*t == "{") {
      while (*t != "}") {
        using namespace selflang::reserved;
        if (*t == var_t) {
          return var_parse(t);
        } else if (*t++ == return_t) {
          if (*t != endl) {
            curr->body.push_back(
                std::make_unique<selflang::ret>(expr_parse(t)));
          }
          curr->body.push_back(std::make_unique<selflang::ret>());
        } else if (reserved_guard(*t)) {
          curr->body.push_back(expr_parse(t));
        }
      }
    }
    return curr;
  }

  void process(token_it t) {
    using namespace selflang::reserved;
    if (*t == var_t) {
      syntax_tree.emplace_back(var_parse(++t));
    } else if (*t == fun_t) {
      syntax_tree.emplace_back(fun_parse(++t));
    } else if (reserved_guard(*t)) {
      syntax_tree.emplace_back(expr_parse(t));
    }
  }

  global_parser() {
    types.insert({selflang::void_type.getName(), &selflang::void_type});
    types.insert({selflang::byte_type.getName(), &selflang::byte_type});
    types.insert({selflang::type_type.getName(), &selflang::type_type});
    types.insert({selflang::int_type.getName(), &selflang::int_type});
    types.insert({selflang::char_type.getName(), &selflang::char_type});
    symbols.push_back(&selflang::int_token_assignment);
    symbols.push_back(&selflang::internal_addi);
    symbols.push_back(&selflang::internal_subi);
    symbols.push_back(&selflang::internal_muli);
    symbols.push_back(&selflang::internal_divi);
    context_stack.push_back(&syntax_tree);
  }
};
static void subtree_pass(selflang::expression_tree &tree, auto begin,
                         auto end) {
  for (auto open = begin; open != end; ++open) {
    if (auto *open_paren =
            dynamic_cast<selflang::unevaluated_expression *>(open->get());
        open_paren && open_paren->get_token() == "(") {
      for (auto close = 1 + open; close != end; ++close) {
        if (auto *close_paren =
                dynamic_cast<selflang::unevaluated_expression *>(
                    close->get())) {
          if (close_paren->get_token() == "(") {
            subtree_pass(tree, close, end);
          } else if (close_paren->get_token() == ")") {
            auto subtree = std::make_unique<selflang::expression_tree>();
            subtree->reserve(std::distance(open + 1, close));
            std::transform(open + 1, close, std::back_inserter(*subtree),
                           [](selflang::expression_ptr &e) {
                             return selflang::expression_ptr(std::move(e));
                           });
            tree.erase(open, close + 1);
            tree.insert(open, std::move(subtree));
          }
        }
      }
    }
  }
  if (tree.nullcheck()) {
    throw std::runtime_error("nullcheck failed");
  }
}
static void subtree_pass(selflang::expression_tree &tree) {
  return subtree_pass(tree, tree.begin(), tree.end());
}
selflang::expression_ptr
global_parser::evaluate_tree(selflang::expression_tree &tree) {
  subtree_pass(tree);
  for (auto &ptr : tree) {
    parse_symbol(ptr);
  }

  // ok not sure why the end check doesn't work but I have to check
  // if the tree is size 1
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    selflang::expression_tree arg;
    if (auto *fun = dynamic_cast<selflang::fun_call *>(it->get())) {
      ++it;
      if (tree.end() == it) {
        break;
      }
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

  auto a = tree.dump();
  // left-right associative pass
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *op = dynamic_cast<selflang::op_call *>(it->get());
        op && !op->get_def().is_member() && op->args.empty()) {
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
        op && op->get_def().is_member() && op->args.empty()) {
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
  parser.process(token_it{in});
  auto result = std::move(parser.syntax_tree);
  return result;
}
} // namespace selflang