#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <memory>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/tuple.hpp"
#include "ast/variables.hpp"
#include "builtins.hpp"

namespace {
llvm::LLVMContext context;
llvm::Value *dispatch(const self::Expression *expr, llvm::IRBuilder<> &builder);
void unpackArgs(self::Expression *e, auto unary) {
  if (auto *args = dynamic_cast<self::arg_pack *>(e)) {
    for (auto &arg : args->members) {
      unary(*arg);
    }
  } else {
    unary(*e);
  }
}
void forEachArg(const self::FunctionCall &base, auto unary) {
  if (base.isBinary()) {
    unpackArgs(base.LHS.get(), unary);
    unpackArgs(base.RHS.get(), unary);
  } else if (base.isUnaryPre()) {
    unpackArgs(base.RHS.get(), unary);
  } else {
    throw std::runtime_error("unimplemented");
  }
}
void forEachArg(const self::FunBase *def, auto unary) {
  if (auto *fun = dynamic_cast<const self::FunctionDef *>(def)) {
    std::for_each(fun->arguments.begin(), fun->arguments.end(), unary);
  } else {
    auto &op = dynamic_cast<const self::OperatorDef &>(*def);
    unary(op.LHS);
    unary(op.RHS);
  }
}

llvm::Type *getType(const self::Type &t) {
  if (&t != &self::type_inst) {
    // return var.type_ref.ptr->
    throw std::runtime_error("non ints not implemented right now");

  } else {
    if (&t == &self::i64_t.value) {
      return llvm::Type::getInt64Ty(context);
    }
    if (&t == &self::char_t.value) {
      return llvm::Type::getInt8Ty(context);
    }
    if (&t == &self::void_t.value) {
      return llvm::Type::getVoidTy(context);
    }
  }
  throw std::runtime_error("non ints not implemented right now");
}
llvm::Type *getType(const self::VarDeclaration &var) {
  return getType(*var.type_ref.ptr);
}
void generateFun(const self::FunBase *fun, llvm::Module &module) {
  std::vector<llvm::Type *> arg_types;
  arg_types.reserve(fun->argcount());
  forEachArg(fun, [&](const std::unique_ptr<self::VarDeclaration> &a) {
    arg_types.push_back(getType(*a));
  });
  auto type =
      llvm::FunctionType::get(getType(*fun->return_type.ptr), arg_types, false);
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
llvm::Value *generateFunCall(const self::FunctionCall &base,
                      llvm::IRBuilder<> &builder) {
  llvm::FunctionType *type;

  {
    std::vector<llvm::Type *> arg_types;
    arg_types.reserve(base.definition.argcount());
    const auto arg_push = [&](const std::unique_ptr<self::VarDeclaration> &a) {
      arg_types.push_back(getType(*a));
    };
    forEachArg(&base.definition, arg_push);
    type = llvm::FunctionType::get(getType(*base.definition.return_type.ptr),
                                   arg_types, false);
  }
  auto &table = builder.GetInsertBlock()->getModule()->getValueSymbolTable();
  auto *val = table.lookup(base.definition.getName());
  auto callee = llvm::FunctionCallee(type, val);
  std::vector<llvm::Value *> args;
  args.reserve(base.definition.argcount());
  forEachArg(base, [&](const self::Expression &e) {
    args.push_back(dispatch(&e, builder));
  });
  return builder.CreateCall(callee, args);
}
llvm::Value *generateCall(const self::FunctionCall &fun,
                          llvm::IRBuilder<> &builder) {
  llvm::Value *result = nullptr;
  switch (self::detail::BuiltinInstruction(fun.get_def().hash)) {
  case self::detail::addi:
    result = builder.CreateAdd(dispatch(fun.LHS.get(), builder),
                               dispatch(fun.RHS.get(), builder));
    break;
  case self::detail::store:
    result = builder.CreateStore(dispatch(fun.RHS.get(), builder),
                                 dispatch(fun.LHS.get(), builder));
    break;
  case self::detail::subi:
    result = builder.CreateSub(dispatch(fun.LHS.get(), builder),
                               dispatch(fun.RHS.get(), builder));
    break;
  case self::detail::muli:
    result = builder.CreateMul(dispatch(fun.LHS.get(), builder),
                               dispatch(fun.RHS.get(), builder));
    break;
  case self::detail::divi:
    result = builder.CreateSDiv(dispatch(fun.LHS.get(), builder),
                                dispatch(fun.RHS.get(), builder));
    break;
  case self::detail::none:
    return generateFunCall(fun, builder);
    break;
  }
  return result;
}

llvm::Value *createString(const self::StringLit &str, llvm::Module &m) {

  auto arr_type =
      llvm::ArrayType::get(llvm::Type::getInt8Ty(context), str.value.size());
  std::vector<llvm::Constant *> constants;
  constants.reserve(str.value.size());
  std::for_each(str.value.begin(), str.value.end(), [&](unsigned char c) {
    constants.push_back(
        llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), c));
  });
  std::array<llvm::Constant *, 2> consts;
  consts[0] =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), str.value.size());
  consts[1] = llvm::ConstantArray::get(arr_type, constants);
  auto type = llvm::StructType::get(
      llvm::Type::getInt64Ty(context),
      llvm::ArrayType::get(llvm::Type::getInt8Ty(context), str.value.size()));

  auto constant = llvm::ConstantStruct::get(type, consts);
  auto global = new llvm::GlobalVariable(
      m, type, true, llvm::GlobalVariable::ExternalLinkage, constant);
  global->setAlignment(llvm::Align(1));
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return global;
}

llvm::Value *dispatch(const self::Expression *expr,
                      llvm::IRBuilder<> &builder) {
  if (auto *fun = dynamic_cast<const self::FunctionCall *>(expr)) {
    return generateCall(*fun, builder);
  } else if (auto &type = typeid(*expr); type == typeid(self::Ret)) {
    auto &ret = dynamic_cast<const self::Ret &>(*expr);
    if (ret.value) {
      return builder.CreateRet(dispatch(ret.value.get(), builder));
    } else {
      return builder.CreateRetVoid();
    }
  } else if (type == typeid(self::IntLit)) {
    return llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(context),
        llvm::APInt(32, dynamic_cast<const self::IntLit &>(*expr).value));
  } else if (type == typeid(self::CharLit)) {
    return llvm::ConstantInt::get(
        llvm::Type::getInt8Ty(context),
        llvm::APInt(8, dynamic_cast<const self::CharLit &>(*expr).value));
  } else if (type == typeid(self::StringLit)) {
    auto &str = dynamic_cast<const self::StringLit &>(*expr);
    return createString(str, *builder.GetInsertBlock()->getModule());
  } else if (type == typeid(self::VarDeclaration)) {
    auto &var = dynamic_cast<const self::VarDeclaration &>(*expr);
    return builder.CreateAlloca(getType(var), 0, var.getName());
  } else {
    std::cerr << type.name() << '\n';
    throw std::runtime_error("I dont know what type this is\n");
  }
}



} // namespace

namespace self {
std::unique_ptr<llvm::Module> codegen(const self::ExpressionTree &ast) {
  auto module = std::make_unique<llvm::Module>(
      "todo: make module name meaningful", context);
  for (auto &expr : ast) { // clang-format off
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wpotentially-evaluated-expression"
    // clang-format on
    if (auto *FunctionDef =
            dynamic_cast<self::FunBase *>(expr.get())) { // clang-format off
    #pragma clang diagnostic pop
      // clang-format on
      generateFun(FunctionDef, *module);
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  return module;
}
} // namespace self