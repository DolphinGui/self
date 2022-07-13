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
#include "ast/tuple.hpp"
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
  bool end() const noexcept { return pos == where.size(); }
};
using token_it = token_it_t<std::vector<selflang::token>>;

struct global_parser {
  static inline std::string err_string;
  // might want to make this a dictionary instead of a list. might scale better
  // for large type lists.
  selflang::type_list types;
  using symbol_map = std::unordered_multimap<selflang::token_view,
                                             const selflang::expression *>;
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
      selflang::expression_tree arg;
      if (auto *fun = dynamic_cast<selflang::fun_call *>(it->get())) {
        ++it;
        if (tree.end() == it) {
          break;
        }
        if (auto *tuple = dynamic_cast<selflang::tuple *>(it->get())) {
          err_assert(tuple->members.size() == fun->definition.arguments.size(),
                     "argument number mismatch");
          // TODO: check types
          std::for_each(tuple->members.begin(), tuple->members.end(),
                        [&](selflang::expression_ptr &ptr) {
                          fun->args.emplace_back(std::move(ptr));
                        });
          it = --tree.erase(it);
        } else {
          fun->args.emplace_back(std::move(*it));
          it = --tree.erase(it);
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
    auto last = selflang::expression_ptr(std::move(tree.back()));
    tree.pop_back();
    return last;
  }

  selflang::expression_ptr parse_symbol(auto &base) {
    if (auto *maybe =
            dynamic_cast<selflang::unevaluated_expression *>(base.get());
        maybe && !maybe->is_complete()) {
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
      auto symbol = symbols.find(maybe->get_token());
      err_assert(symbol != symbols.end(), "Symbol not found");
      if (auto *op =
              dynamic_cast<const selflang::operator_def *>(symbol->second)) {
        return std::make_unique<selflang::op_call>(*op);

      } else if (auto *fun =
                     dynamic_cast<const selflang::fun_def *>(symbol->second)) {
        return std::make_unique<selflang::fun_call>(*fun);
      }
      err_assert(false, "conflicting symbols");
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
      curr->arguments.back()->type.ptr = candidates->second;
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
    symbols.insert({curr->name, curr.get()});
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

  global_parser() {
    types.insert({selflang::void_type.getName(), &selflang::void_type});
    types.insert({selflang::byte_type.getName(), &selflang::byte_type});
    types.insert({selflang::type_type.getName(), &selflang::type_type});
    types.insert({selflang::int_type.getName(), &selflang::int_type});
    types.insert({selflang::char_type.getName(), &selflang::char_type});
    symbols.insert({selflang::int_token_assignment.getName(),
                    &selflang::int_token_assignment});
    symbols.insert(
        {selflang::internal_addi.getName(), &selflang::internal_addi});
    symbols.insert(
        {selflang::internal_subi.getName(), &selflang::internal_subi});
    symbols.insert(
        {selflang::internal_muli.getName(), &selflang::internal_muli});
    symbols.insert(
        {selflang::internal_divi.getName(), &selflang::internal_divi});
  }
};
} // namespace

namespace selflang {
expression_tree parse(token_vec in) {
  global_parser parser;
  return parser.process(token_it{in});
}
} // namespace selflang