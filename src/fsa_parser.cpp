#include <algorithm>
#include <cctype>
#include <cstddef>
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
#include "ast/tuple.hpp"
#include "ast/unevaluated_expression.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"
#include "lexer.hpp"
#include "literals.hpp"
#include "pair_range.hpp"

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
  bool end() const noexcept { return pos == where.size(); }
};
using token_it = token_it_t<std::vector<selflang::token>>;

struct global_parser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  using symbol_map = std::unordered_multimap<selflang::token_view,
                                             selflang::expression_const_ref>;
  symbol_map symbols;
  using self = global_parser;
  // todo: add proper compile error reporting later.
  auto err_assert(bool condition, std::string_view message) {
    if (!condition) {
      err_string = message;
      throw std::runtime_error(err_string);
    }
  }

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
              std::for_each(open + 1, close, [&](selflang::expression_ptr &e) {
                subtree->emplace_back(std::move(e));
              });
              tree.erase(open, close + 1);
              tree.insert(open, std::move(subtree));
            }
          }
        }
      }
    }
  }

  static void subtree_pass(selflang::expression_tree &tree) {
    return subtree_pass(tree, tree.begin(), tree.end());
  }
  void tuple_pass(selflang::expression_tree &tree) {
    size_t size = 0;
    auto tuple = std::make_unique<selflang::tuple>();
    auto mark = tree.begin();
    for (auto it = tree.begin(); it != tree.end(); ++it, ++size) {
      if (auto *comma =
              dynamic_cast<selflang::unevaluated_expression *>(it->get());
          comma && comma->get_token() == ",") {
        selflang::expression_tree branch;
        branch.reserve(size);
        std::for_each(mark, it, [&](selflang::expression_ptr &ptr) {
          branch.push_back(std::move(ptr));
        });
        mark = it = tree.erase(mark, it + 1);
        if (branch.size() > 1) {
          tuple->members.emplace_back(evaluate_tree(branch));
        } else {
          tuple->members.emplace_back(parse_symbol(branch.back()));
        }
        size = 0;
      }
    }
    if (!tuple->members.empty()) {
      if (tree.size() > 1) {
        tuple->members.emplace_back(evaluate_tree(tree));
      } else {
        tuple->members.emplace_back(parse_symbol(tree.back()));
        tree.pop_back();
      }
      tuple->members.shrink_to_fit();
      tree.push_back(std::move(tuple));
    }
  }

  bool type_coerce(selflang::expression *e, selflang::type_ptr type) {
    if (auto *var = dynamic_cast<selflang::var_decl *>(e)) {
      if (!var->type.ptr) {
        var->type = type;
        return true;
      }
    } else if (auto *uneval =
                   dynamic_cast<selflang::unevaluated_expression *>(e)) {
      uneval->coerced_type = type;
      return true;
    }
    return e->getType() == type;
  }

  size_t tuple_count(selflang::expression *e) {
    if (auto *tuple = dynamic_cast<selflang::tuple *>(e))
      return tuple->members.size();
    return 1;
  }
  template <typename T, bool pre = true, bool post = true>
  auto bin_pass(auto &it, bool not_a_member, auto lhsrhsinc, auto cond,
                auto cleanup) {
    if (auto *t = dynamic_cast<selflang::unevaluated_expression *>(it->get())) {
      auto candidates =
          selflang::pair_range(symbols.equal_range(t->get_token()));
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      std::vector<const T *> valid;
      for (auto &[_, fun] : candidates) {
        if (const auto *op = dynamic_cast<const T *>(&fun.get());
            op && (not_a_member ^ op->member)) {
          cond(op, lhs, rhs, valid);
        }
      }
      if (valid.empty())
        return;
      err_assert(valid.size() == 1, "ambiguous operator call");
      auto result = std::make_unique<selflang::op_call>(*valid.back());
      result->LHS = std::move(*lhs);
      result->RHS = std::move(*rhs);
      cleanup(lhs, rhs);
      *it = std::move(result);
    }
  };

  selflang::expression_ptr evaluate_tree(selflang::expression_tree &tree) {
    if (tree.empty()) {
      return std::make_unique<selflang::tuple>();
    }
    subtree_pass(tree);
    for (auto &ptr : tree) {
      if (auto *uneval = dynamic_cast<selflang::expression_tree *>(ptr.get())) {
        ptr = evaluate_tree(*uneval);
      }
    }
    tuple_pass(tree);
    for (auto &ptr : tree) {
      ptr = parse_symbol(ptr);
    }

    // ok not sure why the end check doesn't work but I have to check
    // if the tree is size 1
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      if (auto *t =
              dynamic_cast<selflang::unevaluated_expression *>(it->get())) {
        auto args = it;
        ++args;
        if (args == tree.end())
          break;
        auto candidates =
            selflang::pair_range(symbols.equal_range(t->get_token()));
        std::vector<const selflang::fun_def *> valid;
        for (auto &[_, candidate] : candidates) {
          if (const auto *fun =
                  dynamic_cast<const selflang::fun_def *>(&candidate.get())) {
            // TODO typechecking
            if (fun->argcount() == tuple_count(args->get())) {
              valid.push_back(fun);
            }
          }
        }
        if (valid.empty())
          continue;
        err_assert(valid.size() == 1, "ambiguous operator call");
        auto result = std::make_unique<selflang::op_call>(*valid.back());
        if (auto *tuple = dynamic_cast<selflang::tuple *>(args->get())) {
          result->RHS = std::make_unique<selflang::arg_pack>(std::move(*tuple));
        } else {
          result->RHS = std::move(*args);
        }
        tree.erase(args);
        *it = std::move(result);
      }
    }

    auto dump = tree.dump();

    auto bin_cond = [&](auto *op, auto lhs, auto rhs, auto &valid) {
      if (type_coerce(lhs->get(), op->LHS->decl_type()) &&
          type_coerce(rhs->get(), op->RHS->decl_type())) {
        valid.push_back(op);
      };
    };
    // left-right associative pass
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      bin_pass<selflang::operator_def>(
          it, true, [](auto &lhs, auto &rhs) { --lhs, ++rhs; }, bin_cond,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs);
            it = tree.erase(lhs);
          });
    }

    // right-left associative pass
    for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
      bin_pass<selflang::operator_def>(
          it, false, [](auto &lhs, auto &rhs) { ++lhs, --rhs; }, bin_cond,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs.base());
            it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
          });
    }

    err_assert(tree.size() == 1, "tree is not fully resolved");
    auto last = selflang::expression_ptr(std::move(tree.back()));
    tree.pop_back();
    return last;
  }

  selflang::expression_ptr parse_symbol(selflang::expression_ptr &base) {
    if (auto *maybe =
            dynamic_cast<selflang::unevaluated_expression *>(base.get());
        maybe && !maybe->isComplete()) {
      auto t = maybe->get_token();
      if (auto [is_int, number] = is_integer(t); is_int) {
        return std::make_unique<selflang::int_literal>(number);

      } else if (auto [result, c] = is_char(t); result) {
        return std::make_unique<selflang::char_literal>(c);

      } else if (auto [result, str] = is_str(t); result) {
        std::vector<unsigned char> value;
        value.reserve(str.size() - 1);
        std::copy(str.begin(), str.end() - 1, std::back_inserter(value));
        return std::make_unique<selflang::string_literal>(value);
      }
    }
    return std::move(base);
  }
  using callback = std::function<void(selflang::expression_tree &)>;

  selflang::expression_ptr expr_parse(token_it &t, callback start = nullptr) {
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

  selflang::expression_ptr var_parse(token_it &t) {
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
      curr->type.ptr = &candidates->second.get();
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

  selflang::expression_ptr fun_parse(token_it &t, selflang::token_view name) {
    auto curr = std::make_unique<selflang::fun_def>(name);
    err_assert(*t++ == "(", "\"(\" expected");
    while (*t != ")") {
      err_assert(reserved_guard(*t),
                 "reserved token cannot be used as parameter name");
      curr->arguments.emplace_back(std::make_unique<selflang::var_decl>(*t++));
      err_assert(*t++ == ":", "\":\" expected here");
      // todo make this an expression instead of hardcoded
      auto candidates = types.find(*t++);
      err_assert(candidates != types.end(), "no types found");
      curr->arguments.back()->type.ptr = &candidates->second.get();
      err_assert(*t == ")" || *t == ",", "\")\" or \",\" expected");
      if (*t == ")")
        break;
      else
        ++t;
    }
    if (*++t == "->") {
      ++t;
      auto candidates = types.find(*t++);
      err_assert(candidates != types.end(), "type not found");
      curr->return_type.ptr = &candidates->second.get();
      curr->body_defined = false;
    }
    if (*t == "{") {
      while (*t != "}") {
        using namespace selflang::reserved;
        if (*t == var_t) {
          curr->body.push_back(var_parse(++t));
        } else if (*t++ == return_t) {
          if (*t != endl) {
            curr->body.push_back(
                std::make_unique<selflang::ret>(expr_parse(t)));
          } else {
            curr->body.push_back(std::make_unique<selflang::ret>());
          }
        } else if (reserved_guard(*t)) {
          curr->body.push_back(expr_parse(t));
        }
      }
      curr->body_defined = true;
    }
    ++t;
    symbols.insert({curr->name, *curr});
    return curr;
  }

  selflang::expression_ptr struct_parse(token_it &t) {
    err_assert(*t == "{" || *t == "(", "Expected a \"{\" or \"(\"");
    if (*t == "(") {
      ++t;
      if (*t == ")") {
        return std::make_unique<selflang::opaque_struct>();
      } else {
        auto [success, size] = is_integer(*t);
        err_assert(success, "Expected integer or \")\"");
        return std::make_unique<selflang::opaque_struct>(size);
      }
    } else {
      auto result = std::make_unique<selflang::struct_def>();
      while (*t != "}") {
        if (*(++t) == "var") {
          result->body.push_back(var_parse(t));
        } else if (*t == "fun") {
          auto name = *++t;
          err_assert(reserved_guard(name), "function name is reserved");
          result->body.push_back(fun_parse(t, name));
        } else {
          err_assert(false, "expected a function or variable declaration.");
        }
      }
      return result;
    }
  }

  selflang::expression_tree process(token_it t) {
    selflang::expression_tree syntax_tree;
    using namespace selflang::reserved;
    while (!t.end()) {
      if (*t == var_t) {
        syntax_tree.emplace_back(var_parse(++t));
      } else if (*t == fun_t) {
        auto name = *++t;
        err_assert(reserved_guard(name), "function name is reserved");
        syntax_tree.emplace_back(fun_parse(++t, name));
      } else if (reserved_guard(*t)) {
        syntax_tree.emplace_back(expr_parse(t));
      } else {
        err_assert(*t++ == endl, "invalid expression");
      }
    }
    return syntax_tree;
  }

  global_parser() : symbols() {
    const auto type_inserter = [this](const selflang::var_decl &var) {
      types.insert({var.getName(), std::cref(var)});
    };
    type_inserter(selflang::void_type);
    type_inserter(selflang::byte_type);
    type_inserter(selflang::type_type);
    type_inserter(selflang::int_type);
    type_inserter(selflang::char_type);
    const auto symbol_inserter = [this](const selflang::expression &expr) {
      symbols.insert({expr.getName(), std::cref(expr)});
    };
    symbol_inserter(selflang::int_token_assignment);
    symbol_inserter(selflang::internal_addi);
    symbol_inserter(selflang::internal_subi);
    symbol_inserter(selflang::internal_muli);
    symbol_inserter(selflang::internal_divi);
  }
};
} // namespace

namespace selflang {
expression_tree parse(token_vec in) {
  global_parser parser;
  return parser.process(token_it{in});
}
} // namespace selflang