#include <algorithm>
#include <bit>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <memory>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "builtins.hpp"
#include "syntax_tree.hpp"

namespace selflang {
llvm::Type *getType(const var_decl &);
}
namespace {
llvm::LLVMContext context;
auto module = llvm::Module("todo: make module name meaningful", context);

llvm::Value *dispatch(selflang::expression *expr, llvm::IRBuilder<> &builder);

llvm::Value *fun_call_gen(selflang::op_call &fun, llvm::IRBuilder<> &builder) {
  llvm::Value *result = nullptr;
  switch (selflang::detail::hash_value(fun.get_def().hash)) {
  case selflang::detail::addi:
    result = builder.CreateAdd(dispatch(fun.args.at(0).get(), builder),
                               dispatch(fun.args.at(1).get(), builder));
    break;
  case selflang::detail::store:
    result = builder.CreateStore(dispatch(fun.args.at(1).get(), builder),
                                 dispatch(fun.args.at(0).get(), builder));
    break;
  case selflang::detail::subi:
    result = builder.CreateSub(dispatch(fun.args.at(0).get(), builder),
                               dispatch(fun.args.at(1).get(), builder));
    break;
  case selflang::detail::muli:
    result = builder.CreateMul(dispatch(fun.args.at(0).get(), builder),
                               dispatch(fun.args.at(1).get(), builder));
    break;
  case selflang::detail::divi:
    result = builder.CreateSDiv(dispatch(fun.args.at(0).get(), builder),
                                dispatch(fun.args.at(1).get(), builder));
    break;
  case selflang::detail::none:
    throw std::runtime_error(
        "calling other functions is unimplemented for now.");
    break;
  }
  return result;
}

llvm::Value *dispatch(selflang::expression *expr, llvm::IRBuilder<> &builder) {
  if (auto &type = typeid(*expr); type == typeid(selflang::op_call)) {
    return fun_call_gen(dynamic_cast<selflang::op_call &>(*expr), builder);
  } else if (type == typeid(selflang::ret)) {
    auto &ret = dynamic_cast<selflang::ret &>(*expr);
    if (ret.value) {
      return builder.CreateRet(dispatch(ret.value.get(), builder));
    } else {
      return builder.CreateRetVoid();
    }
  } else if (type == typeid(selflang::int_literal)) {
    return llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(context),
        llvm::APInt(32, dynamic_cast<selflang::int_literal &>(*expr).value));
  } else if (type == typeid(selflang::var_decl)) {
    auto &var = dynamic_cast<selflang::var_decl &>(*expr);
    return builder.CreateAlloca(getType(var), 0, var.getName());
  } else {
    std::cerr << type.name() << '\n';
    throw std::runtime_error("I dont know what type this is\n");
  }
}

void fun_gen(selflang::fun_def *fun) {
  std::vector<llvm::Type *> arg_types;
  arg_types.reserve(fun->arguments.size());
  std::transform(
      fun->arguments.begin(), fun->arguments.end(),
      std::back_inserter(arg_types),
      [](std::unique_ptr<selflang::var_decl> &a) { return getType(*a); });
  auto type =
      llvm::FunctionType::get(getType(*fun->return_type), arg_types, false);
  auto result = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                       fun->getName(), module);
  if (fun->body_defined) {
    auto block = llvm::BasicBlock::Create(context, "entry", result);
    auto builder = llvm::IRBuilder<>(context);
    builder.SetInsertPoint(block);
    for (auto &a : fun->body) {
      dispatch(a.get(), builder);
    }
  }
}

} // namespace

namespace selflang {
llvm::Module &codegen(const selflang::expression_tree &ast) {
  for (auto &expr : ast) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpotentially-evaluated-expression"

    if (auto &type = typeid(*expr); type == typeid(selflang::fun_def)) {

#pragma GCC diagnostic pop
      fun_gen(dynamic_cast<selflang::fun_def *>(expr.get()));
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  return module;
}
llvm::Type *getType(const var_decl &var) {
  if (var.type.ptr != &type_type) {
    return getType(*var.type.ptr);
  } else {
    if (&var == &int_type) {
      return llvm::Type::getInt32Ty(context);
    }
    if (&var == &char_type) {
      return llvm::Type::getInt8Ty(context);
    }
  }
  throw std::runtime_error("non ints not implemented right now");
}
} // namespace selflang