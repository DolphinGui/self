#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cxxabi.h>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
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
#include "ast/visitor.hpp"
#include "builtins.hpp"

namespace {
using namespace self;
// I swear this is proof c++
// needs virtual function params
struct Generator : ExprVisitor {
private:
  llvm::LLVMContext &context;
  self::Context &c;
  std::unordered_map<std::string, llvm::AllocaInst *> var_map{};
  llvm::StructType *str_type;
  // std::vector<llvm::DIScope *> stack;
  // llvm::DIFile *file;
  llvm::Module &module;
  struct Params {
    const self::ExprBase *expr;
    llvm::IRBuilder<> &builder;
    bool return_val;
    llvm::Value *ret = nullptr;
  };

public:
  Generator(Context &c, llvm::LLVMContext &context, llvm::Module &m)
      : context(context), c(c), module(m) {
    std::array<llvm::Type *, 2> members = {llvm::Type::getInt64Ty(context),
                                           llvm::PointerType::get(context, 0)};
    str_type = llvm::StructType::get(context, members);
    // stack.push_back(file);
  }
  void generateFun(const self::FunBase &fun, self::Context &c,
                   llvm::DIBuilder &di) {

    std::vector<llvm::Type *> arg_types;
    arg_types.reserve(fun.argcount());
    forEachArg(fun, [&](const std::unique_ptr<self::VarDeclaration> &a) {
      arg_types.push_back(getType(*a, c));
    });

    auto type =
        llvm::FunctionType::get(getType(fun.return_type, c), arg_types, false);
    auto result = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                         fun.getForeignName(), module);
    result->setCallingConv(llvm::CallingConv::C);
    if (fun.body_defined) {
      auto block = llvm::BasicBlock::Create(context, "entry", result);
      auto builder = llvm::IRBuilder<>(context);
      builder.SetInsertPoint(block);
      // builder.SetCurrentDebugLocation(llvm::DILocation::get())
      for (auto &a : *fun.body) {
        dispatch(a.get(), builder);
      }
    }
  }

private:
  llvm::Value *dispatch(const self::ExprBase *expr, llvm::IRBuilder<> &builder,
                        bool return_val = true) {
    auto passthrough = Params{expr, builder, return_val};
    expr->visit(*this, &passthrough);
    return passthrough.ret;
  }

  void operator()(const ExprBase &expr, void *data) override {
    auto &type = typeid(expr);
    std::cerr << abi::__cxa_demangle(type.name(), nullptr, nullptr, nullptr)
              << '\n';
    throw std::runtime_error("I dont know what type this is\n");
  }

  void operator()(const Ret &ret, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    if (ret.value) {
      d.ret = builder.CreateRet(dispatch(ret.value.get(), builder));
    } else {
      d.ret = builder.CreateRetVoid();
    }
  }

  void operator()(const FunctionCall &fun, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    switch (fun.getDefinition().internal) {
    case self::detail::addi:
      d.ret = builder.CreateAdd(dispatch(fun.lhs.get(), builder),
                                dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::store:
      d.ret = builder.CreateStore(dispatch(fun.rhs.get(), builder),
                                  dispatch(fun.lhs.get(), builder, false));
      break;

    case self::detail::subi:
      d.ret = builder.CreateSub(dispatch(fun.lhs.get(), builder),
                                dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::muli:
      d.ret = builder.CreateMul(dispatch(fun.lhs.get(), builder),
                                dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::divi:
      d.ret = builder.CreateSDiv(dispatch(fun.lhs.get(), builder),
                                 dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::call:
      d.ret = generateFunCall(fun, builder);
      break;

    case self::detail::assign:
    case self::detail::cmpeq:
      d.ret = builder.CreateICmpEQ(dispatch(fun.lhs.get(), builder),
                                   dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::cmpneq:
      d.ret = builder.CreateICmpNE(dispatch(fun.lhs.get(), builder),
                                   dispatch(fun.rhs.get(), builder));
      break;

    case self::detail::addr:
      d.ret = dispatch(fun.rhs.get(), builder, false);
      break;

    case self::detail::assignaddr:
      d.ret = builder.CreateStore(dispatch(fun.rhs.get(), builder, false),
                                  dispatch(fun.lhs.get(), builder, false));
      break;

    default:
      throw std::runtime_error("This shouldn't happen");
    }
  }

  void operator()(const IntLit &lit, void *data) override {
    auto &d = *static_cast<Params *>(data);
    d.ret = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),
                                   llvm::APInt(64, lit.value, true));
  }

  void operator()(const StringLit &str, void *data) override {
    auto &d = *static_cast<Params *>(data);
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
        module, type, true, llvm::GlobalVariable::ExternalLinkage,
        str_constant);
    global->setAlignment(llvm::Align(1));
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    d.ret = llvm::ConstantStruct::get(str_type, {size, global});
  }

  void operator()(const BoolLit &lit, void *data) override {
    auto &d = *static_cast<Params *>(data);
    llvm::ConstantInt::getBool(llvm::Type::getInt1Ty(context), lit.value);
  }

  void operator()(const VarDeclaration &var, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto result =
        d.builder.CreateAlloca(getType(var, c), 0, var.getDemangledName());
    var_map.insert({var.getDemangledName(), result});
    d.ret = result;
  }

  void operator()(const VarDeref &var, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto result = var_map.at(self::VarDeclaration::demangle(var.getName()));
    if (var.definition.getDecltype().depth > 0) {
      d.ret = d.builder.CreateLoad(llvm::PointerType::get(context, 0), result);
      return;
    }
    if (!d.return_val) {
      d.ret = result;
    } else {
      d.ret = d.builder.CreateLoad(result->getAllocatedType(), result);
    }
  }

  void operator()(const If &if_e, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    auto function = builder.GetInsertBlock()->getParent();
    auto then_block = llvm::BasicBlock::Create(context, "then", function);
    auto cont_block = llvm::BasicBlock::Create(context, "continued");
    llvm::BasicBlock *else_block;
    if (if_e.else_block) {
      else_block = llvm::BasicBlock::Create(context, "else");
    } else {
      else_block = cont_block;
    }
    builder.CreateCondBr(dispatch(if_e.condition.get(), builder), then_block,
                         else_block);
    builder.SetInsertPoint(then_block);
    for (auto &e : *if_e.block) {
      dispatch(e.get(), builder);
    }
    builder.CreateBr(cont_block);
    function->getBasicBlockList().push_back(else_block);

    if (else_block != cont_block) {
      builder.SetInsertPoint(else_block);
      for (auto &e : *if_e.else_block) {
        dispatch(e.get(), builder);
      }
      builder.CreateBr(cont_block);
    }

    if (else_block != cont_block) {
      function->getBasicBlockList().push_back(cont_block);
    }
    builder.SetInsertPoint(cont_block);
  }

  void operator()(const While &while_e, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    auto function = builder.GetInsertBlock()->getParent();
    auto loop = llvm::BasicBlock::Create(context, "while loop", function);
    auto cont_block = llvm::BasicBlock::Create(context, "after loop");
    if (while_e.is_do) {
      builder.CreateBr(loop);
    } else {
      builder.CreateCondBr(dispatch(while_e.condition.get(), builder), loop,
                           cont_block);
    }
    builder.SetInsertPoint(loop);
    for (auto &e : *while_e.block) {
      dispatch(e.get(), builder);
    }
    builder.CreateCondBr(dispatch(while_e.condition.get(), builder), loop,
                         cont_block);
    function->getBasicBlockList().push_back(cont_block);
    builder.SetInsertPoint(cont_block);
    throw std::runtime_error("unimplemented right now");
  }

  llvm::Value *generateFunCall(const self::FunctionCall &base,
                               llvm::IRBuilder<> &builder) {
    llvm::FunctionType *type;
    {
      std::vector<llvm::Type *> arg_types;
      arg_types.reserve(base.definition.argcount());
      const auto arg_push =
          [&](const std::unique_ptr<self::VarDeclaration> &a) {
            arg_types.push_back(getType(*a, c));
          };
      forEachArg(base.definition, arg_push);
      type = llvm::FunctionType::get(getType(base.definition.return_type, c),
                                     arg_types, false);
    }
    auto &table = builder.GetInsertBlock()->getModule()->getValueSymbolTable();
    auto *val = table.lookup(base.definition.getForeignName());
    auto callee = llvm::FunctionCallee(type, val);
    std::vector<llvm::Value *> args;
    args.reserve(base.definition.argcount());
    forEachArg(base, [&](const self::ExprBase &e) {
      args.push_back(
          dispatch(&e, builder, e.getType().is_ref == self::RefTypes::value));
    });
    return builder.CreateCall(callee, args);
  }
  void unpackArgs(self::ExprBase *e, auto unary) {
    if (auto *args = dynamic_cast<self::ArgPack *>(e)) {
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
  void forEachArg(const self::FunBase &def, auto unary) {
    if (auto *fun = dynamic_cast<const self::FunctionDef *>(&def)) {
      std::for_each(fun->arguments.begin(), fun->arguments.end(), unary);
    } else {
      auto &op = dynamic_cast<const self::OperatorDef &>(def);
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
};
} // namespace

namespace self {
// must be returned by unique_ptr because module cannot be moved or copied
std::unique_ptr<llvm::Module> codegen(const self::ExprTree &ast, Context &c,
                                      llvm::LLVMContext &llvm,
                                      std::filesystem::path file) {
  auto module =
      std::make_unique<llvm::Module>("todo: make module name meaningful", llvm);
  auto g = Generator(c, llvm, *module);
  auto di = llvm::DIBuilder(*module);
  // auto compile_unit = di.createCompileUnit(
  //     llvm::dwarf::DW_LANG_C,
  //     di.createFile(file.filename().c_str(), file.parent_path().c_str()),
  //     "SELF compiler", false, "", 0);
  // g.file =
  //     di.createFile(compile_unit->getFilename(),
  //     compile_unit->getDirectory());
  for (auto &expr : ast) { // clang-format off
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wpotentially-evaluated-expression"
    // clang-format on
    if (auto *FunctionDef =
            dynamic_cast<self::FunBase *>(expr.get())) { // clang-format off
    #pragma clang diagnostic pop
      // clang-format on
      g.generateFun(*FunctionDef, c, di);
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  return module;
}
} // namespace self