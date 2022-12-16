#include "ast/tuple.hpp"
#include "parser.hpp"
#include <span>
using namespace self::detail::parser;
namespace {

size_t tuple_count(self::ExprBase *e) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(e))
    return tuple->members.size();
  return 1;
}

void for_tuple(self::ExprPtr &e, auto unary) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(e.get())) {
    for (auto &a : tuple->members) {
      unary(*a);
    }
  } else
    unary(e);
}

void for_tuple(self::ExprPtr &left, auto gen, auto binary) {
  if (auto *tuple = dynamic_cast<self::Tuple *>(left.get())) {
    for (auto member = tuple->members.begin(); member != tuple->members.end();
         ++member) {
      binary(*member, gen());
    }
  } else
    binary(left, gen());
}

self::ExprPtr evaluateTree(self::Context &c, self::ExprTree &tree,
                           self::Index &local);

void processTuples(self::Context &c, self::ExprTree &tree,
                   self::Index &context) {
  size_t size = 0;
  auto tuple = self::makeExpr<self::Tuple>(tree.at(0)->pos);
  auto mark = tree.begin();
  for (auto it = tree.begin(); it != tree.end(); ++it, ++size) {
    if (auto *comma = dynamic_cast<self::UnevaluatedExpression *>(it->get());
        comma && comma->getToken() == ",") {
      self::ExprTree branch;
      branch.reserve(size);
      std::for_each(mark, it, [&](self::ExprPtr &ptr) {
        branch.push_back(std::move(ptr));
      });
      mark = it = tree.erase(mark, it + 1);
      tuple->members.emplace_back(evaluateTree(c, branch, context));
      size = 0;
    }
  }
  if (!tuple->members.empty()) {
    tuple->members.emplace_back(evaluateTree(c, tree, context));
    tuple->members.shrink_to_fit();
    tree.clear();
    tree.push_back(std::move(tuple));
  }
}

self::ExprPtr evaluateTree(self::Context &c, self::ExprTree &tree,
                           self::Index &local);
// notably there is only ever 1 tuple in the tree
// since recursive tuples are processed first recursively

std::optional<bool> isBool(self::TokenView t) {
  if (t == "true")
    return true;
  else if (t == "false")
    return false;
  else
    return std::nullopt;
}

void tryCoerceFun(auto &args, auto &ctor, auto end, auto arg_it, auto &perfect,
                  auto &coerced, self::Context &c) {
  for (auto arg : args) {
    if (arg_it == end)
      return;
    auto type = arg_it->get()->getDecltype();
    if (auto r = needCoerce(c, arg, type); r == coerceResult::mismatch)
      return;
    else {
      if (r == coerceResult::match) {
        perfect.push_back(&ctor);
        return;
      } else {
        coerced.push_back(&ctor);
        return;
      }
    }
  }
}

self::FunctionDef *findCtor(self::StructDef &structure,
                            std::span<self::ExprBase *> args, self::Context &c,
                            self::Pos p) {
  auto candidates =
      structure.context->localEqualRange(self::FunctionDef::qualify("struct"));
  std::vector<self::FunctionDef *> perfect;
  std::vector<self::FunctionDef *> coerced;
  for (auto &[_, f] : self::pairRange(candidates)) {
    auto &ctor = dynamic_cast<self::FunctionDef &>(f.get());
    if (ctor.argcount() != args.size())
      continue;
    tryCoerceFun(args, ctor, ctor.arguments.end(), ctor.arguments.begin(),
                 perfect, coerced, c);
  }
  if (!perfect.empty()) {
    errReport(perfect.size() == 1, p, "ambiguous constructor call");
    return perfect.back();
  } else {
    errReport(!coerced.empty(), p, "No valid constructor calls");
    errReport(coerced.size() == 1, p, "ambiguous constructor call");
    return coerced.back();
  }
}

self::ExprPtr ctorCheck(auto it, auto &container, auto &var, self::Context &c) {
  auto next = lookahead(it, container).value();
  // checks for constructor call
  auto *structure = dynamic_cast<self::StructLit *>(var.value);
  auto pos = var.pos;
  if (var.getDecltype().ptr != &c.type_t || !*next || !structure) {
    return self::makeExpr<self::VarDeref>(pos, var);
  }
  self::FunctionDef *ctor = nullptr;
  if (auto next_type = next->get()->getType(); next_type.ptr) {
    auto args = std::array{next->get()};
    ctor = findCtor(structure->value, args, c, pos);
    coerceType(*next, ctor->arguments.back()->getDecltype());
  } else if (auto *tuple = dynamic_cast<self::Tuple *>(next->get())) {
    std::vector<self::ExprBase *> exprs;
    exprs.reserve(tuple->members.size());
    std::for_each(tuple->members.begin(), tuple->members.end(),
                  [&](self::ExprPtr &e) { exprs.push_back(e.get()); });
    ctor = findCtor(structure->value, exprs, c, pos);
    auto arg_pack =
        self::makeExpr<self::ArgPack>(tuple->pos, std::move(*tuple));
    auto decl = ctor->arguments.begin();
    auto arg = arg_pack->members.begin();
    while (decl != ctor->arguments.end() && arg != arg_pack->members.end()) {
      coerceType(*arg, decl->get()->getDecltype());
      ++decl, ++arg;
    }
    *next = std::move(arg_pack);
  } else {
    return self::makeExpr<self::VarDeref>(pos, var);
  }
  auto result = self::makeExpr<self::FunctionCall>(pos, *ctor);
  result->rhs = std::move(*next);
  container.erase(next);
  return result;
}

self::ExprPtr parseSymbol(auto it, auto &container, self::Index &local,
                          self::Context &c) {
  auto pos = it->get()->pos;
  if (auto *maybe = dynamic_cast<self::UnevaluatedExpression *>(it->get());
      maybe && !maybe->isComplete()) {
    auto t = maybe->getToken();
    if (auto number = isInt(t)) {
      return self::makeExpr<self::IntLit>(pos, *number, c);
    } else if (auto str = isStr(t)) {
      return self::makeExpr<self::StringLit>(pos, *str, c);
    } else if (auto boolean = isBool(t)) {
      return self::makeExpr<self::BoolLit>(pos, *boolean, c);
    } else if (self::BuiltinTypeLit::contains(t, c)) {
      return self::makeExpr<self::BuiltinTypeLit>(
          pos, self::BuiltinTypeLit::get(t, c));
    } else if (auto varname = self::VarDeclaration::qualify(t);
               local.contains(varname)) {
      errReport(local.isUnique(varname), pos,
                "ODR var declaration rule violated");
      auto &var =
          dynamic_cast<self::VarDeclaration &>(local.find(varname)->get());
      return ctorCheck(it, container, var, c);
    }
  }
  return std::move(*it);
}

void resolveMembers(self::ExprTree &container) {
  for (auto it = container.begin(); it != container.end(); ++it) {
    if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get());
        t && t->getToken() == ".") {
      auto behind = lookbehind(it, container).value();
      auto &behind_ptr = expectCast<self::VarDeref &>(
          **behind, "Dot operator expects a variable "
                    "dereference to the left");
      auto &struct_def = expectCast<const self::StructDef &>(
          *behind_ptr.definition.value->getType().ptr,
          "Dot operator should be used on structure type");
      auto ahead = expectLookahead(it, container);
      auto &ahead_e = expectCast<self::UnevaluatedExpression &>(
          **ahead, "Unknown parsing error: Uneval expected");
      auto ahead_token = ahead_e.getToken();
      auto ahead_pos = ahead->get()->pos;
      auto n = self::VarDeclaration::qualify(ahead_token);
      if (auto v = struct_def.context->findLocally(n)) {
        auto structure = upCast<self::VarDeref>(std::move(*behind));
        auto &var = dynamic_cast<self::VarDeclaration &>(v->get());
        self::ExprPtr n = self::makeExpr<self::MemberDeref>(
            ahead_pos, var, std::move(structure), struct_def);
        behind->swap(n);
      } else if (auto f = struct_def.context->findLocally(
                     self::FunctionDef::qualify(ahead_token))) {
        auto &fun = dynamic_cast<self::FunctionDef &>(v->get());
        errReport(fun.arguments.front()->getRawName() == "this", ahead_pos,
                  "Object-oriented-style function calls require 'this' "
                  "parameter as first parameter");
        auto arg_list = self::makeExpr<self::ArgPack>(ahead->get()->pos);
        arg_list->members.reserve(fun.argcount());
        arg_list->members.push_back(std::move(*behind));
        auto call = self::makeExpr<self::FunctionCall>(ahead_pos, fun);
        call->rhs = std::move(arg_list);
        *behind = std::move(call);
      }
      container.erase(ahead);
      container.erase(it);
      it = behind;
    }
  }
}

template <typename T, bool pre = true, bool post = true>
auto processFunction(auto &it, bool not_a_member, auto lhsrhsinc, auto cond,
                     auto cleanup, auto insert, auto coerce,
                     self::Index &context) {
  if (auto *t = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
    auto lhs = it, rhs = it;
    lhsrhsinc(lhs, rhs);
    std::vector<const T *> no_coerce;
    std::vector<const T *> coerced;
    auto search = [&](auto candidates) {
      for (auto &[_, fun] : candidates) {
        const auto &op = dynamic_cast<const T &>(fun.get());
        if (not_a_member ^ op.member) {
          cond(&op, lhs, rhs, no_coerce, coerced);
        }
      }
    };
    // argument dependent lookup
    search(self::pairRange(context.equalRange(T::qualify(t->getToken()))));
    // constructor search
    for (auto &hs : {lhs, rhs}) {
      if (auto *str =
              dynamic_cast<const self::StructDef *>(hs->get()->getType().ptr)) {
        search(self::pairRange(
            str->context->localEqualRange(T::qualify(t->getToken()))));
      }
    }
    if (!no_coerce.empty()) {
      errReport(no_coerce.size() == 1, it->get()->pos,
                "ambiguous operator call");
      coerce(*no_coerce.back(), *lhs, *rhs);
      auto result = insert(no_coerce.back(), lhs, rhs, no_coerce.back()->pos);
      cleanup(lhs, rhs);
      *it = std::move(result);
    } else if (!coerced.empty()) {
      errReport(coerced.size() == 1, it->get()->pos, "ambiguous operator call");
      coerce(*coerced.back(), *lhs, *rhs);
      auto result = insert(coerced.back(), lhs, rhs, coerced.back()->pos);
      cleanup(lhs, rhs);
      *it = std::move(result);
    }
  }
}

void processSubtrees(self::ExprTree &tree, auto begin, auto end) {
  for (auto open = begin; open != end; ++open) {
    if (auto *open_paren =
            dynamic_cast<self::UnevaluatedExpression *>(open->get());
        open_paren && open_paren->getToken() == "(") {
      for (auto close = 1 + open; close != end; ++close) {
        if (auto *close_paren =
                dynamic_cast<self::UnevaluatedExpression *>(close->get())) {
          if (close_paren->getToken() == "(") {
            processSubtrees(tree, close, end);
          } else if (close_paren->getToken() == ")") {
            auto subtree = self::makeExpr<self::ExprTree>({open_paren->pos});
            subtree->reserve(std::distance(open + 1, close));
            std::for_each(open + 1, close, [&](self::ExprPtr &e) {
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

void processSubtrees(self::ExprTree &tree) {
  return processSubtrees(tree, tree.begin(), tree.end());
}

self::ExprPtr evaluateTree(self::Context &c, self::ExprTree &tree,
                           self::Index &local) {
  if (tree.empty()) {
    return self::makeExpr<self::Tuple>(tree.pos);
  }
  processSubtrees(tree);
  for (auto &ptr : tree) {
    if (auto *uneval = dynamic_cast<self::ExprTree *>(ptr.get())) {
      ptr = evaluateTree(c, *uneval, local);
    }
  }

  processTuples(c, tree, local);
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    if (auto *p = dynamic_cast<self::UnevaluatedExpression *>(it->get())) {
      if (p->getToken() == ".") {
        ++it;
        continue;
      }
    }
    *it = parseSymbol(it, tree, local, c);
  }
  resolveMembers(tree);
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    processFunction<self::FunctionDef>(
        it, true, [](auto &, auto &rhs) { ++rhs; },
        // todo add type checking to fun param
        [](auto *fun, auto, auto rhs, auto &perfect, auto &) {
          if (fun->argcount() == tuple_count(rhs->get())) {
            perfect.push_back(fun);
            return true;
          }
          return false;
        },
        [&](auto, auto rhs) { tree.erase(rhs); },
        [&](const self::FunctionDef *fun, auto, auto rhs, self::Pos p) {
          auto result = self::makeExpr<self::FunctionCall>(p, *fun);
          if (auto *tuple = dynamic_cast<self::Tuple *>(rhs->get())) {
            result->rhs = std::make_unique<self::ArgPack>(std::move(*tuple));
          } else {
            result->rhs = std::move(*rhs);
          }
          return result;
        },
        [](const self::FunctionDef &fun, auto &, auto &rhs) {
          int i = 0;
          for_tuple(
              rhs, [&] { return fun.arguments.at(i++)->getDecltype(); },
              [](self::ExprPtr &e, auto type) { coerceType(e, type); });
        },
        local);
  }

  auto binCondition = [&](auto *op, auto lhs, auto rhs, auto &no_coerce,
                          auto &coerce_r) {
    using enum coerceResult;
    auto left = needCoerce(c, lhs->get(), op->lhs->getDecltype());
    auto right = needCoerce(c, rhs->get(), op->rhs->getDecltype());
    if (left == match && right == match) {
      no_coerce.push_back(op);
      return true;
    } else if (left != mismatch && right != mismatch) {
      coerce_r.push_back(op);
      return true;
    }
    return false;
  };
  auto binInsert = [](const self::OperatorDef *o, auto lhs, auto rhs,
                      self::Pos p) {
    auto result = self::makeExpr<self::FunctionCall>(p, *o);
    if (auto *var = dynamic_cast<self::VarDeclaration *>(lhs->get())) {
      var->value = rhs->get();
    }
    result->lhs = std::move(*lhs);
    result->rhs = std::move(*rhs);
    return result;
  };
  auto bin_coerce = [&](const self::OperatorDef &fun, auto &lhs, auto &rhs) {
    if (fun.internal == self::detail::assign ||
        fun.internal == self::detail::store) {
      auto &var = dynamic_cast<self::VarDeclaration &>(*lhs);
      var.value = rhs.get();
    }
    if (auto n = fun.lhs->getDecltype(); n.ptr == &c.deduced_t) {
      coerceType(lhs, rhs.get()->getType());
    } else {
      coerceType(lhs, n);
    }
    coerceType(rhs, fun.rhs->getDecltype());
  };
  // left-right associative pass
  for (auto it = tree.begin(); it != tree.end(); ++it) {
    processFunction<self::OperatorDef>(
        it, true, [](auto &lhs, auto &rhs) { --lhs, ++rhs; }, binCondition,
        [&](auto lhs, auto rhs) {
          tree.erase(rhs);
          it = tree.erase(lhs);
        },
        binInsert, bin_coerce, local);
  }

  // right-left associative pass
  for (auto it = tree.rbegin(); it != tree.rend(); ++it) {
    processFunction<self::OperatorDef>(
        it, false, [](auto &lhs, auto &rhs) { ++lhs, --rhs; }, binCondition,
        [&](auto lhs, auto rhs) {
          tree.erase(rhs.base());
          it = std::make_reverse_iterator(++tree.erase(--lhs.base()));
        },
        binInsert, bin_coerce, local);
  }
  errReport(tree.size() == 1, tree.pos, "tree is not fully resolved");
  return std::move(tree.back());
}
} // namespace
namespace self::detail::parser {

self::ExprPtr
GlobalParser::parseExpr(TokenIt &t, self::Index &context,
                        GlobalParser::callback start,
                        std::function<bool(self::TokenView)> endExpr) {
  self::ExprTree tree;
  if (start) {
    start(tree);
  }
  while (!endExpr(*t)) {
    if (*t == self::reserved::struct_t) {
      tree.push_back(parseStruct(++t, context));
    } else if (*t == self::reserved::var_t) {
      auto name = *++t;
      tree.push_back(parseVar(++t, name, context));
    } else {
      tree.push_back(
          self::makeExpr<self::UnevaluatedExpression>({t.col, t.line}, *t++));
    }
  }
  return self::foldExpr(evaluateTree(c, tree, context), context).first;
}

} // namespace self::detail::parser