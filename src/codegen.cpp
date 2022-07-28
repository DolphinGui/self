#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cxxabi.h>
#include <iostream>
#include <iterator>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
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
struct Generator {
  llvm::LLVMContext &context;
  std::unordered_map<std::string, llvm::AllocaInst *> var_map{};
  llvm::StructType *str_type;

  Generator(llvm::LLVMContext &context) : context(context) {
    std::array<llvm::Type *, 2> members = {llvm::Type::getInt64Ty(context),
                                           llvm::PointerType::get(context, 0)};
    str_type = llvm::StructType::get(context, members);
  }

  void unpackArgs(self::ExprBase *e, auto unary) {
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
      unpackArgs(base.lhs.get(), unary);
      unpackArgs(base.rhs.get(), unary);
    } else if (base.isUnaryPre()) {
      unpackArgs(base.rhs.get(), unary);
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  void forEachArg(const self::FunBase *def, auto unary) {
    if (auto *fun = dynamic_cast<const self::FunctionDef *>(def)) {
      std::for_each(fun->arguments.begin(), fun->arguments.end(), unary);
    } else {
      auto &op = dynamic_cast<const self::OperatorDef &>(*def);
      unary(op.lhs);
      unary(op.rhs);
    }
  }

  llvm::Type *getType(self::TypeRef t, self::Context &c) {
    if (t.is_ref == self::RefTypes::ref) {
      return llvm::PointerType::get(context, 0);
    }
    if (t == c.i64_t) {
      return llvm::Type::getInt64Ty(context);
    }
    if (t == c.u64_t) {
      return llvm::Type::getInt64Ty(context);
    }
    if (t == c.void_t) {
      return llvm::Type::getVoidTy(context);
    }
    if (t == c.bool_t) {
      return llvm::Type::getInt1Ty(context);
    }
    if (t == c.str_t) {
      return str_type;
    }

    throw std::runtime_error("non ints not implemented right now");
  }
  llvm::Type *getType(const self::VarDeclaration &var, self::Context &c) {
    return getType(var.getDecltype(), c);
  }
  void generateFun(const self::FunBase *fun, llvm::Module &module,
                   self::Context &c) {
    std::vector<llvm::Type *> arg_types;
    arg_types.reserve(fun->argcount());
    forEachArg(fun, [&](const std::unique_ptr<self::VarDeclaration> &a) {
      arg_types.push_back(getType(*a, c));
    });

    auto type =
        llvm::FunctionType::get(getType(fun->return_type, c), arg_types, false);
    auto result = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                         fun->getForeignName(), module);
    result->setCallingConv(llvm::CallingConv::C);
    if (fun->body_defined) {
      auto block = llvm::BasicBlock::Create(context, "entry", result);
      auto builder = llvm::IRBuilder<>(context);
      builder.SetInsertPoint(block);
      for (auto &a : *fun->body) {
        dispatch(a.get(), builder, c);
      }
    }
  }
  llvm::Value *generateFunCall(const self::FunctionCall &base,
                               llvm::IRBuilder<> &builder, self::Context &c) {
    llvm::FunctionType *type;
    {
      std::vector<llvm::Type *> arg_types;
      arg_types.reserve(base.definition.argcount());
      const auto arg_push =
          [&](const std::unique_ptr<self::VarDeclaration> &a) {
            arg_types.push_back(getType(*a, c));
          };
      forEachArg(&base.definition, arg_push);
      type = llvm::FunctionType::get(getType(base.definition.return_type, c),
                                     arg_types, false);
    }
    auto &table = builder.GetInsertBlock()->getModule()->getValueSymbolTable();
    auto *val = table.lookup(base.definition.getForeignName());
    auto callee = llvm::FunctionCallee(type, val);
    std::vector<llvm::Value *> args;
    args.reserve(base.definition.argcount());
    forEachArg(base, [&](const self::ExprBase &e) {
      args.push_back(dispatch(&e, builder, c,
                              e.getType().is_ref == self::RefTypes::value));
    });
    return builder.CreateCall(callee, args);
  }
  llvm::Value *generateCall(const self::FunctionCall &fun,
                            llvm::IRBuilder<> &builder, self::Context &c) {
    switch (fun.getDefinition().internal) {
    case self::detail::addi:
      return builder.CreateAdd(dispatch(fun.lhs.get(), builder, c),
                               dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::store:
      return builder.CreateStore(dispatch(fun.rhs.get(), builder, c),
                                 dispatch(fun.lhs.get(), builder, c, false));
      break;
    case self::detail::subi:
      return builder.CreateSub(dispatch(fun.lhs.get(), builder, c),
                               dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::muli:
      return builder.CreateMul(dispatch(fun.lhs.get(), builder, c),
                               dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::divi:
      return builder.CreateSDiv(dispatch(fun.lhs.get(), builder, c),
                                dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::call:
      return generateFunCall(fun, builder, c);
      break;
    case self::detail::assign:
    case self::detail::cmpeq:
      return builder.CreateICmpEQ(dispatch(fun.lhs.get(), builder, c),
                                  dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::cmpneq:
      return builder.CreateICmpNE(dispatch(fun.lhs.get(), builder, c),
                                  dispatch(fun.rhs.get(), builder, c));
      break;
    case self::detail::addr:
      return dispatch(fun.rhs.get(), builder, c);
    default:
      throw std::runtime_error("This shouldn't happen");
    }
    return nullptr;
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
    auto size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                       str.value.size());
    auto str_constant = llvm::ConstantArray::get(arr_type, constants);
    auto type =
        llvm::ArrayType::get(llvm::Type::getInt8Ty(context), str.value.size());
    auto global = new llvm::GlobalVariable(
        m, type, true, llvm::GlobalVariable::ExternalLinkage, str_constant);
    global->setAlignment(llvm::Align(1));
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return llvm::ConstantStruct::get(str_type, {size, global});
  }

  void createWhile(const self::While *while_e, llvm::IRBuilder<> &builder,
                   self::Context &c) {
    auto function = builder.GetInsertBlock()->getParent();
    auto loop = llvm::BasicBlock::Create(context, "while loop", function);
    auto cont_block = llvm::BasicBlock::Create(context, "after loop");
    if (while_e->is_do) {
      builder.CreateBr(loop);
    } else {
      builder.CreateCondBr(dispatch(while_e->condition.get(), builder, c), loop,
                           cont_block);
    }
    builder.SetInsertPoint(loop);
    for (auto &e : *while_e->block) {
      dispatch(e.get(), builder, c);
    }
    builder.CreateCondBr(dispatch(while_e->condition.get(), builder, c), loop,
                         cont_block);
    function->getBasicBlockList().push_back(cont_block);
    builder.SetInsertPoint(cont_block);
  }

  void createIf(const self::If *if_e, llvm::IRBuilder<> &builder,
                self::Context &c) {
    auto function = builder.GetInsertBlock()->getParent();
    auto then_block = llvm::BasicBlock::Create(context, "then", function);
    auto cont_block = llvm::BasicBlock::Create(context, "continued");
    llvm::BasicBlock *else_block;
    if (if_e->else_block) {
      else_block = llvm::BasicBlock::Create(context, "else");
    } else {
      else_block = cont_block;
    }
    builder.CreateCondBr(dispatch(if_e->condition.get(), builder, c),
                         then_block, else_block);
    builder.SetInsertPoint(then_block);
    for (auto &e : *if_e->block) {
      dispatch(e.get(), builder, c);
    }
    builder.CreateBr(cont_block);
    function->getBasicBlockList().push_back(else_block);

    if (else_block != cont_block) {
      builder.SetInsertPoint(else_block);
      for (auto &e : *if_e->else_block) {
        dispatch(e.get(), builder, c);
      }
      builder.CreateBr(cont_block);
    }

    if (else_block != cont_block) {
      function->getBasicBlockList().push_back(cont_block);
    }
    builder.SetInsertPoint(cont_block);
  }

  llvm::Value *dispatch(const self::ExprBase *expr, llvm::IRBuilder<> &builder,
                        self::Context &c, bool return_val = true) {
    if (auto *fun = dynamic_cast<const self::FunctionCall *>(expr)) {
      return generateCall(*fun, builder, c);
    } else if (auto &type = typeid(*expr); type == typeid(self::Ret)) {
      auto &ret = dynamic_cast<const self::Ret &>(*expr);
      if (ret.value) {
        return builder.CreateRet(dispatch(ret.value.get(), builder, c));
      } else {
        return builder.CreateRetVoid();
      }
    } else if (type == typeid(self::IntLit)) {
      return llvm::ConstantInt::get(
          llvm::Type::getInt64Ty(context),
          llvm::APInt(64, dynamic_cast<const self::IntLit &>(*expr).value,
                      true));
    } else if (type == typeid(self::StringLit)) {
      auto &str = dynamic_cast<const self::StringLit &>(*expr);
      return createString(str, *builder.GetInsertBlock()->getModule());
    } else if (type == typeid(self::BoolLit)) {
      return llvm::ConstantInt::getBool(
          llvm::Type::getInt1Ty(context),
          dynamic_cast<const self::BoolLit &>(*expr).value);
    } else if (type == typeid(self::VarDeclaration)) {
      auto &var = dynamic_cast<const self::VarDeclaration &>(*expr);
      auto t = getType(var, c);
      auto result = builder.CreateAlloca(t, 0, var.getDemangledName());
      auto t2 = result->getAllocatedType();
      auto t3 = result->getType();
      var_map.insert({var.getDemangledName(), result});
      return result;
    } else if (type == typeid(self::VarDeref)) {
      auto var = dynamic_cast<const self::VarDeref &>(*expr);
      auto result = var_map.at(self::VarDeclaration::demangle(var.getName()));
      if (!return_val)
        return result;
      else
        return builder.CreateLoad(result->getAllocatedType(), result);
    } else if (type == typeid(self::If)) {
      createIf(dynamic_cast<const self::If *>(expr), builder, c);
      return nullptr;
    } else if (type == typeid(self::While)) {
      createWhile(dynamic_cast<const self::While *>(expr), builder, c);
      return nullptr;
    } else {
      std::cerr << abi::__cxa_demangle(type.name(), nullptr, nullptr, nullptr)
                << '\n';
      throw std::runtime_error("I dont know what type this is\n");
    }
  }
};

} // namespace

namespace self {
// must be returned by unique_ptr because module cannot be moved or copied
std::unique_ptr<llvm::Module> codegen(const self::ExprTree &ast, Context &c,
                                      llvm::LLVMContext &llvm) {
  auto module =
      std::make_unique<llvm::Module>("todo: make module name meaningful", llvm);
  auto g = Generator(llvm);
  for (auto &expr : ast) { // clang-format off
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wpotentially-evaluated-expression"
    // clang-format on
    if (auto *FunctionDef =
            dynamic_cast<self::FunBase *>(expr.get())) { // clang-format off
    #pragma clang diagnostic pop
      // clang-format on
      g.generateFun(FunctionDef, *module, c);
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  return module;
}
} // namespace self