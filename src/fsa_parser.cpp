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
#include "scope_guard.hpp"

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
  std::vector<std::string> curr_scope;
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
  enum struct coerce_result { match, coerce, mismatch };
  coerce_result need_coerce(const selflang::expression *e,
                            selflang::type_ptr type) {
    using enum coerce_result;
    if (const auto *var = dynamic_cast<const selflang::var_decl *>(e)) {
      if (!var->type.ptr) {
        return coerce;
      }
    } else if (const auto *uneval =
                   dynamic_cast<const selflang::unevaluated_expression *>(e)) {
      return coerce;
    }
    if (e->getType() == type)
      return match;
    else
      return mismatch;
  }

  static bool type_coerce(selflang::expression *e, selflang::type_ptr type) {
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

  static size_t tuple_count(selflang::expression *e) {
    if (auto *tuple = dynamic_cast<selflang::tuple *>(e))
      return tuple->members.size();
    return 1;
  }

  static void for_tuple(selflang::expression *e, auto unary) {
    if (auto *tuple = dynamic_cast<selflang::tuple *>(e)) {
      for (auto &a : tuple->members) {
        unary(*a);
      }
    }
    unary(*e);
  }
  static void for_tuple(selflang::expression *left, auto gen, auto binary) {
    if (auto *tuple = dynamic_cast<selflang::tuple *>(left)) {
      for (auto l = tuple->members.begin(); l != tuple->members.end(); ++l) {
        binary(**l, gen());
      }
      throw std::runtime_error("for tuple mismatch");
    }
    binary(*left, gen());
  }
  template <typename T, bool pre = true, bool post = true>
  auto bin_pass(auto &it, bool not_a_member, auto lhsrhsinc, auto cond,
                auto cleanup, auto insert, auto coerce) {
    if (auto *t = dynamic_cast<selflang::unevaluated_expression *>(it->get())) {
      auto candidates =
          selflang::pair_range(symbols.equal_range(t->get_token()));
      auto lhs = it, rhs = it;
      lhsrhsinc(lhs, rhs);
      std::vector<const T *> no_coerce;
      std::vector<const T *> corced;
      auto count = std::distance(candidates.begin(), candidates.end());
      for (auto &[_, fun] : candidates) {
        if (const auto *op = dynamic_cast<const T *>(&fun.get());
            op && (not_a_member ^ op->member)) {
          cond(op, lhs, rhs, no_coerce, corced);
        }
      }
      if (!no_coerce.empty()) {
        err_assert(no_coerce.size() == 1, "ambiguous operator call");
        auto result = insert(no_coerce.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      } else if (!corced.empty()) {
        err_assert(corced.size() == 1, "ambiguous operator call");
        coerce(*corced.back(), *lhs, *rhs);
        auto result = insert(corced.back(), lhs, rhs);
        cleanup(lhs, rhs);
        *it = std::move(result);
      }
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
    } // clang-format off

    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wunused-parameter"
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      bin_pass<selflang::fun_def>(
          it, true, [](auto &lhs, auto &rhs) { ++rhs; },
          [](auto *fun, auto lhs, auto rhs,
             auto &perfect, auto& less_than) {
            if (fun->argcount() == tuple_count(rhs->get())) {
              perfect.push_back(fun);
            }
          },
          [&](auto lhs, auto rhs) { tree.erase(rhs); },
          [&](const selflang::fun_def *fun, auto lhs, auto rhs) {
            auto result = std::make_unique<selflang::op_call>(*fun);
            if (auto *tuple = dynamic_cast<selflang::tuple *>(rhs->get())) {
              result->RHS =
                  std::make_unique<selflang::arg_pack>(std::move(*tuple));
            } else {
              result->RHS = std::move(*rhs);
            }
            return result;
          }, [](const selflang::fun_def& fun, auto& lhs, auto& rhs){
            int i = 0;
            for_tuple(rhs.get(), [&]{
              return fun.arguments.at(i++)->decl_type();},
              [](selflang::expression& e, auto type){type_coerce(&e, type);});
          });
    }
    #pragma clang diagnostic pop // clang-format on

    auto dump = tree.dump();

    auto bin_cond = [&](auto *op, auto lhs, auto rhs, auto &no_coerce,
                        auto &coerce_r) {
      using enum coerce_result;
      auto left = need_coerce(lhs->get(), op->LHS->decl_type());
      auto right = need_coerce(rhs->get(), op->RHS->decl_type());
      if (left == match && right == match) {
        no_coerce.push_back(op);
      } else if (left != mismatch && right != mismatch) {
        coerce_r.push_back(op);
      }
    };
    auto bin_insert = [](const selflang::operator_def *o, auto lhs, auto rhs) {
      auto result = std::make_unique<selflang::op_call>(*o);
      result->LHS = std::move(*lhs);
      result->RHS = std::move(*rhs);
      return result;
    };
    auto bin_coerce = [](const selflang::operator_def &fun, auto &lhs,
                         auto &rhs) {
      type_coerce(lhs.get(), fun.LHS->decl_type());
      type_coerce(rhs.get(), fun.RHS->decl_type());
    };
    // left-right associative pass
    for (auto it = tree.begin(); it != tree.end(); ++it) {
      bin_pass<selflang::operator_def>(
          it, true, [](auto &lhs, auto &rhs) { --lhs, ++rhs; }, bin_cond,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs);
            it = tree.erase(lhs);
          },
          bin_insert, bin_coerce);
    }

    // right-left associative pass
    for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
      bin_pass<selflang::operator_def>(
          it, false, [](auto &lhs, auto &rhs) { ++lhs, --rhs; }, bin_cond,
          [&](auto lhs, auto rhs) {
            tree.erase(rhs.base());
            it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
          },
          bin_insert, bin_coerce);
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
      if (*t == selflang::reserved::struct_t) {
        tree->push_back(struct_parse(++t));
      } else if (*t == selflang::reserved::var_t) {
      } else {
        tree->push_back(
            std::make_unique<selflang::unevaluated_expression>(*t++));
      }
    }
    auto result = evaluate_tree(*tree);
    return result;
  }

  selflang::expression_ptr var_parse(token_it &t, selflang::token_view name) {
    using namespace selflang::reserved;
    const auto guard = [this](auto t) {
      if (!reserved_guard(t)) {
        std::stringstream err;
        err << "token " << t << " is reserved";
        err_assert(false, err.str());
      }
    };
    guard(name);
    auto curr = std::make_unique<selflang::var_decl>(name);
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
    auto g = selflang::scope_guard([&]() { curr_scope.emplace_back(name); },
                                   [&]() noexcept { curr_scope.pop_back(); });
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
          auto name = *++t;
          curr->body.push_back(var_parse(++t, name));
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
        ++t;
        return std::make_unique<selflang::opaque_struct_literal>(0);
      } else {
        auto [success, size] = is_integer(*t++);
        err_assert(success, "Expected integer or \")\"");
        return std::make_unique<selflang::opaque_struct_literal>(size);
      }
    } else {
      auto result = selflang::struct_def();
      while (*t != "}") {
        if (*(++t) == "var") {
          auto name = *++t;
          result.body.push_back(var_parse(++t, name));
        } else if (*t == "fun") {
          auto name = *++t;
          err_assert(reserved_guard(name), "function name is reserved");
          result.body.push_back(fun_parse(t, name));
        } else {
          err_assert(*t != selflang::reserved::endl,
                     "expected a function or variable declaration.");
        }
      }
      ++t;
      return std::make_unique<selflang::struct_literal>(std::move(result));
    }
  }

  selflang::expression_tree process(token_it t) {
    selflang::expression_tree syntax_tree;
    using namespace selflang::reserved;
    while (!t.end()) {
      if (*t == var_t) {
        auto name = *++t;
        syntax_tree.emplace_back(var_parse(++t, name));
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
    symbol_inserter(selflang::i32_assignment);
    symbol_inserter(selflang::addi);
    symbol_inserter(selflang::subi);
    symbol_inserter(selflang::muli);
    symbol_inserter(selflang::divi);
    symbol_inserter(selflang::struct_assignment);
  }
};
} // namespace

namespace selflang {
expression_tree parse(token_vec in) {
  global_parser parser;
  return parser.process(token_it{in});
}
} // namespace selflang