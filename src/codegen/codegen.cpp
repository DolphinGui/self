#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
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
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <memory>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include <cxxabi.h>

#include "ast/control.hpp"
#include "ast/expression.hpp"
#include "ast/expression_tree.hpp"
#include "ast/functions.hpp"
#include "ast/literal.hpp"
#include "ast/struct_def.hpp"
#include "ast/tuple.hpp"
#include "ast/variables.hpp"
#include "ast/visitor.hpp"
#include "backend_config.hpp"
#include "builtins.hpp"

namespace {
llvm::DIType *strDI(llvm::Module &module, llvm::DIBuilder &di,
                    llvm::StructType *str_type) {
  auto data_layout = module.getDataLayout();
  auto ptrbits = data_layout.getPointerSizeInBits();
  auto str_layout = data_layout.getStructLayout(str_type);
  auto offsets = str_layout->getElementOffsetInBits(1);
  auto ptr = di.createMemberType(
      nullptr, "raw_str", nullptr, 0, ptrbits, 0, 0,
      llvm::DINode::DIFlags::FlagArtificial,
      di.createPointerType(
          di.createBasicType("", 8, llvm::dwarf::DW_ATE_signed_char), ptrbits));
  auto size = di.createMemberType(
      nullptr, "size", nullptr, 0, ptrbits, 0, offsets,
      llvm::DINode::DIFlags::FlagArtificial,
      di.createBasicType("i64", 64, llvm::dwarf::DW_ATE_unsigned));
  auto str = di.createStructType(
      nullptr, "str", nullptr, 0, str_layout->getSizeInBits(), 64,
      llvm::DINode::DIFlags::FlagArtificial, nullptr, {});
  di.replaceArrays(str, di.getOrCreateArray({ptr, size}));
  return str;
}

// llvm::GlobalVariable *createArr(llvm::Module &module,
//                                 std::span<llvm::Function *> ctors,
//                                 llvm::LLVMContext &llvm) {
//   auto type = llvm::StructType::get(llvm, {llvm::Type::getInt32Ty(llvm),
//                                            llvm::PointerType::get(llvm, 0),
//                                            llvm::PointerType::get(llvm, 0)});
//   auto ctor_type = llvm::ArrayType::get(type, ctors.size());
//   auto ctor_arr = std::vector<llvm::Constant *>();
//   ctor_arr.reserve(ctors.size());
//   auto ptr_type = llvm::PointerType::get(llvm, 0);
//   auto *max = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm), 65535);
//   std::ranges::for_each(ctors, [&](llvm::Function *f) {
//     ctor_arr.push_back(llvm::ConstantStruct::get(
//         type, {max, f, llvm::ConstantPointerNull::get(ptr_type)}));
//   });
//   auto global = new llvm::GlobalVariable(
//       ctor_type, true, llvm::GlobalVariable::AppendingLinkage,
//       llvm::ConstantArray::get(ctor_type, ctor_arr), "llvm.global_ctors");
//   return global;
// }
using namespace self;

// I swear this is proof c++
// needs virtual function params
struct Generator : ExprVisitor {
private:
  llvm::LLVMContext &llvm;
  self::Context &context;

  std::unordered_map<const self::VarDeclaration *, llvm::GlobalVariable *>
      global_map{};
  std::vector<llvm::DIScope *> stack;
  llvm::Module &module;
  llvm::DIBuilder &di;
  llvm::DIFile *file;
  llvm::DICompileUnit *compile_unit;
  llvm::StructType *str_type;
  llvm::DIType *str_ditype;
  llvm::FunctionType *ctor_type;
  std::vector<llvm::Function *> ctor_list;
  std::unordered_map<const self::StructDef *, llvm::StructType *> struct_map;
  std::unordered_map<const self::FunBase *, llvm::Function *> fun_map;

  using VarMap =
      std::unordered_map<const self::VarDeclaration *, llvm::Value *>;
  struct Params {
    const self::ExprBase *expr;
    llvm::IRBuilder<> &builder;
    VarMap &vars;
    bool return_by_value;
    llvm::Value *ret = nullptr;
  };

  llvm::DIScope *sp() {
    if (stack.size() == 1)
      return nullptr;
    else
      return stack.back();
  }

public:
  Generator(Context &c, llvm::LLVMContext &context, llvm::Module &m,
            llvm::DIBuilder &di, std::filesystem::path filepath,
            std::span<const self::StructDef *const> struct_list)
      : llvm(context), context(c), module(m), di(di),
        str_type(
            llvm::StructType::get(context, {llvm::PointerType::get(context, 0),
                                            llvm::Type::getInt64Ty(context)})),
        file(di.createFile(filepath.filename().c_str(),
                           filepath.parent_path().c_str())),
        compile_unit(di.createCompileUnit(llvm::dwarf::DW_LANG_C, file,
                                          "SELF compiler", false, "", 0)) {
    stack.push_back(compile_unit);
    str_ditype = strDI(module, di, str_type);
    ctor_type = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm), false);
    for (const auto str : struct_list) {
      makeStructType(*str);
    }
  }

  void generateFun(const self::FunBase &fun) {
    createFun(fun, fun.getForeignName());
  }

  void generateGlobal(const self::VarDeclaration &var) {
    if (var.getType().ptr == &context.type_t) {
      return;
    }
    auto global = new llvm::GlobalVariable(module, getType(var), false,
                                           llvm::GlobalVariable::PrivateLinkage,
                                           nullptr, var.getRawName());
    global_map.insert({&var, global});
    global->setInitializer(getZeroed(var.getDecltype()));
    if (var.value) {
      auto initfun = llvm::Function::Create(
          ctor_type, llvm::Function::PrivateLinkage, "", module);
      auto block = llvm::BasicBlock::Create(llvm, "entry", initfun);
      auto builder = llvm::IRBuilder<>(llvm);
      builder.SetInsertPoint(block);
      VarMap v;
      builder.CreateStore(dispatch(var.value, builder, v), global);
      builder.CreateRetVoid();
      ctor_list.push_back(initfun);
    }
  }

private:
  // explicit return type is for breaking a dependency loop
  // when lazily evaluating constructors
  void createFun(const self::FunBase &fun, std::string_view foreign,
                 llvm::Type *return_type = nullptr) {
    std::vector<llvm::Type *> arg_types;
    arg_types.reserve(fun.argcount());
    forEachArg(fun, [&](const std::unique_ptr<self::VarDeclaration> &a) {
      arg_types.push_back(getType(*a));
    });
    if (!return_type) {
      return_type = getType(fun.return_type);
    }
    auto type = llvm::FunctionType::get(return_type, arg_types, false);
    auto linkage = llvm::Function::LinkageTypes::InternalLinkage;
    if (fun.isExternal()) {
      linkage = llvm::Function::LinkageTypes::ExternalLinkage;
    }
    auto result = llvm::Function::Create(type, linkage, foreign, module);
    result->setCallingConv(llvm::CallingConv::C);

    // if it is neither defined internally nor externally, it must
    // be a compiler generated function
    if (!fun.body_defined && !fun.isExternal()) {
      createDefaultedFunction(fun, result);
    } else if (fun.body_defined) {
      auto subprogram = di.createFunction(
          file, fun.getDequalified(), foreign, file, fun.pos.line,
          createFunDebugType(fun), fun.pos.line, llvm::DINode::FlagPrototyped,
          llvm::DISubprogram::SPFlagDefinition);
      stack.push_back(subprogram);
      auto block = llvm::BasicBlock::Create(llvm, "entry", result);
      auto builder = llvm::IRBuilder<>(llvm);
      result->setSubprogram(subprogram);
      builder.SetInsertPoint(block);
      emitLocation(fun.body->pos, builder);
      VarMap vars;
      // this depends on args being stored in
      // the same order as created
      auto arg_val = result->args().begin();
      fun.iterateArgs([&](const VarDeclaration &var) {
        auto &arg = *arg_val;
        vars.insert({&var, &arg});
      });
      for (auto &a : *fun.body) {
        dispatch(a.get(), builder, vars);
      }
      stack.pop_back();
      di.finalizeSubprogram(subprogram);
    }
    fun_map.insert({&fun, result});
  }

  // only generates code
  void createDefaultedFunction(const self::FunBase &fun,
                               llvm::Function *result) {
    auto name = fun.getDequalified();
    auto block = llvm::BasicBlock::Create(llvm, "entry", result);
    auto builder = llvm::IRBuilder<>(llvm);
    builder.SetInsertPoint(block);
    // default constructor
    if (name == "struct") {
      std::vector<llvm::Value *> args;
      args.reserve(fun.argcount());
      for (auto &arg : result->args()) {
        args.push_back(&arg);
      }
      auto ret_type = result->getReturnType();
      auto alloca = builder.CreateAlloca(ret_type);
      int i = 0;
      for (auto *arg : args) {
        auto element = builder.CreateStructGEP(ret_type, alloca, i++);
        builder.CreateStore(arg, element);
      }
      auto ret_value = builder.CreateLoad(ret_type, alloca);
      builder.CreateRet(ret_value);
      // default assignment
    } else if (name == "=") {
      builder.CreateStore(result->getArg(1), result->getArg(0));
      builder.CreateRet(result->getArg(0));
    }
  }

  llvm::Value *dispatch(const self::ExprBase *expr, llvm::IRBuilder<> &builder,
                        VarMap &vars, bool return_val = true) {
    auto passthrough = Params{expr, builder, vars, return_val};
    emitLocation(*expr, builder);
    expr->visit(*this, &passthrough);
    return passthrough.ret;
  }

  // todo throw typeid
  void operator()(const ExprBase &expr, void *data) override {
    // auto &type = typeid(expr);
    // std::cerr << abi::__cxa_demangle(type.name(), nullptr, nullptr, nullptr)
    //           << '\n';
    throw std::runtime_error("I dont know what type this is\n");
  }

  void operator()(const Ret &ret, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    if (ret.value) {
      d.ret = builder.CreateRet(dispatch(ret.value.get(), builder, d.vars));
    } else {
      d.ret = builder.CreateRetVoid();
    }
  }

  void operator()(const FunctionCall &fun, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    switch (fun.getDefinition().internal) {
    case self::detail::addi:
      d.ret = builder.CreateAdd(dispatch(fun.lhs.get(), builder, d.vars),
                                dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::store:
      d.ret =
          builder.CreateStore(dispatch(fun.rhs.get(), builder, d.vars),
                              dispatch(fun.lhs.get(), builder, d.vars, false));
      break;

    case self::detail::subi:
      d.ret = builder.CreateSub(dispatch(fun.lhs.get(), builder, d.vars),
                                dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::muli:
      d.ret = builder.CreateMul(dispatch(fun.lhs.get(), builder, d.vars),
                                dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::divi:
      d.ret = builder.CreateSDiv(dispatch(fun.lhs.get(), builder, d.vars),
                                 dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::call:
      d.ret = generateFunCall(fun, builder, d.vars);
      break;

    case self::detail::cmpeq:
      d.ret = builder.CreateICmpEQ(dispatch(fun.lhs.get(), builder, d.vars),
                                   dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::cmpneq:
      d.ret = builder.CreateICmpNE(dispatch(fun.lhs.get(), builder, d.vars),
                                   dispatch(fun.rhs.get(), builder, d.vars));
      break;

    case self::detail::addr:
      d.ret = dispatch(fun.rhs.get(), builder, d.vars, false);
      break;

    case self::detail::assignaddr:
      d.ret =
          builder.CreateStore(dispatch(fun.rhs.get(), builder, d.vars, false),
                              dispatch(fun.lhs.get(), builder, d.vars, false));
      break;

    case self::detail::assign:
      break;
    default:
      throw std::runtime_error("This shouldn't happen");
    }
  }

  void operator()(const IntLit &lit, void *data) override {
    auto &d = *static_cast<Params *>(data);
    d.ret = llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm), lit.value);
  }

  void operator()(const StringLit &str, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto arr_type =
        llvm::ArrayType::get(llvm::Type::getInt8Ty(llvm), str.value.size());
    std::vector<llvm::Constant *> constants;
    constants.reserve(str.value.size());
    std::for_each(str.value.begin(), str.value.end(), [&](unsigned char c) {
      constants.push_back(
          llvm::ConstantInt::get(llvm::Type::getInt8Ty(llvm), c));
    });
    auto size =
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm), str.value.size());
    auto str_constant = llvm::ConstantArray::get(arr_type, constants);
    auto type =
        llvm::ArrayType::get(llvm::Type::getInt8Ty(llvm), str.value.size());
    auto global = new llvm::GlobalVariable(
        module, type, true, llvm::GlobalVariable::ExternalLinkage,
        str_constant);
    global->setAlignment(llvm::Align(1));
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    d.ret = llvm::ConstantStruct::get(str_type, {global, size});
  }

  void operator()(const BoolLit &lit, void *data) override {
    auto &d = *static_cast<Params *>(data);
    llvm::ConstantInt::getBool(llvm::Type::getInt1Ty(llvm), lit.value);
  }

  void operator()(const VarDeclaration &var, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto line = var.pos.line, col = var.pos.col;
    auto result = d.builder.CreateAlloca(getType(var), 0, var.getRawName());
    d.vars.insert({&var, result});
    if (auto back = sp()) {
      auto n = di.createAutoVariable(back, var.getRawName(), file, line,
                                     getDIType(var.getDecltype()));
      di.insertDeclare(result, n, di.createExpression(),
                       llvm::DILocation::get(llvm, line, col, back),
                       d.builder.GetInsertBlock());
    }
    d.ret = result;
  }

  void operator()(const VarDeref &var, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto result = d.vars.at(&var.definition);
    // not sure if this is strictly necessary
    if (var.definition.getDecltype().depth > 0) {
      auto load = d.builder.CreateLoad(llvm::PointerType::get(llvm, 0), result);
      d.ret = load;
      return;
    }
    if (d.return_by_value) {
      if (auto alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(result)) {
        d.ret = d.builder.CreateLoad(alloca_inst->getAllocatedType(), result);
        return;
      }
      d.ret = result;
    } else {
      assert(llvm::isa<llvm::AllocaInst>(result));
      d.ret = result;
    }
  }

  void operator()(const If &if_e, void *data) override {
    auto &d = *static_cast<Params *>(data);
    auto &builder = d.builder;
    auto function = builder.GetInsertBlock()->getParent();
    auto then_block = llvm::BasicBlock::Create(llvm, "then", function);
    auto cont_block = llvm::BasicBlock::Create(llvm, "continued");
    llvm::BasicBlock *else_block;
    if (if_e.else_block) {
      else_block = llvm::BasicBlock::Create(llvm, "else");
    } else {
      else_block = cont_block;
    }
    builder.CreateCondBr(dispatch(if_e.condition.get(), builder, d.vars),
                         then_block, else_block);
    builder.SetInsertPoint(then_block);
    for (auto &e : *if_e.block) {
      dispatch(e.get(), builder, d.vars);
    }
    builder.CreateBr(cont_block);
    function->getBasicBlockList().push_back(else_block);

    if (else_block != cont_block) {
      builder.SetInsertPoint(else_block);
      for (auto &e : *if_e.else_block) {
        dispatch(e.get(), builder, d.vars);
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
    auto loop = llvm::BasicBlock::Create(llvm, "while loop", function);
    auto cont_block = llvm::BasicBlock::Create(llvm, "after loop");
    if (while_e.is_do) {
      builder.CreateBr(loop);
    } else {
      builder.CreateCondBr(dispatch(while_e.condition.get(), builder, d.vars),
                           loop, cont_block);
    }
    builder.SetInsertPoint(loop);
    for (auto &e : *while_e.block) {
      dispatch(e.get(), builder, d.vars);
    }
    builder.CreateCondBr(dispatch(while_e.condition.get(), builder, d.vars),
                         loop, cont_block);
    function->getBasicBlockList().push_back(cont_block);
    builder.SetInsertPoint(cont_block);
    throw std::runtime_error("unimplemented right now");
  }

  static size_t getElementIndex(const StructDef &def,
                                const VarDeclaration &member) {
    size_t result = 0;
    for (const auto &m : def.body) {
      if (auto var = dynamic_cast<const self::VarDeclaration *const>(m.get())) {
        if (&member == var) {
          return result;
        }
        result++;
      }
    }
    throw std::runtime_error("member not found");
  }

  // todo make this work for struct params
  void operator()(const MemberDeref &deref, void *d) override {
    auto &params = *static_cast<Params *>(d);
    auto result = params.vars.at(&deref.structure.get()->definition);
    auto type = getType(deref.type);
    auto index = getElementIndex(deref.type, deref.definition);
    auto ptr = params.builder.CreateStructGEP(type, result, index);
    if (deref.definition.getDecltype().depth > 0) {
      params.ret = ptr;
      return;
    }
    if (params.return_by_value) {
      if (auto alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(result)) {
        params.ret =
            params.builder.CreateLoad(type->getStructElementType(index), ptr);
        return;
      }
      // todo:
      throw std::runtime_error("literal struct dereferences not done yet");
    } else {
      assert(llvm::isa<llvm::AllocaInst>(result));
      params.ret = ptr;
    }
  }

  llvm::Value *generateFunCall(const self::FunctionCall &base,
                               llvm::IRBuilder<> &builder, VarMap &v) {
    llvm::FunctionType *type;
    {
      std::vector<llvm::Type *> arg_types;
      arg_types.reserve(base.definition.argcount());
      const auto arg_push =
          [&](const std::unique_ptr<self::VarDeclaration> &a) {
            arg_types.push_back(getType(*a));
          };
      forEachArg(base.definition, arg_push);
      type = llvm::FunctionType::get(getType(base.definition.return_type),
                                     arg_types, false);
    }
    auto &table = builder.GetInsertBlock()->getModule()->getValueSymbolTable();
    auto *val = fun_map.at(&base.getDefinition());
    auto callee = llvm::FunctionCallee(type, val);
    std::vector<llvm::Value *> args;
    args.reserve(base.definition.argcount());
    forEachArg(base, [&](const self::ExprBase &e) {
      args.push_back(dispatch(&e, builder, v,
                              e.getType().is_ref == self::RefTypes::value));
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

  llvm::Type *getType(self::TypeRef t) {
    if (t.is_ref == self::RefTypes::ref) {
      return llvm::PointerType::get(llvm, 0);
    }
    if (t == context.i64_t) {
      return llvm::Type::getInt64Ty(llvm);
    }
    if (t == context.u64_t) {
      return llvm::Type::getInt64Ty(llvm);
    }
    if (t == context.void_t) {
      return llvm::Type::getVoidTy(llvm);
    }
    if (t == context.bool_t) {
      return llvm::Type::getInt1Ty(llvm);
    }
    if (t == context.str_t) {
      return str_type;
    }
    if (auto structure = dynamic_cast<const self::StructDef *>(&t.ptr)) {
      return struct_map.at(structure);
    }

    std::string n =
        abi::__cxa_demangle(typeid(t.ptr).name(), nullptr, nullptr, nullptr);
    throw std::runtime_error(" not implemented right now");
  }

  llvm::Type *getType(const self::VarDeclaration &var) {
    return getType(var.getDecltype());
  }

  void makeStructType(const self::StructDef &str) {
    std::vector<llvm::Type *> members;
    std::vector<const self::FunBase *> functions;
    members.reserve(str.body.size());
    functions.reserve(str.body.size());
    for (const auto &member : str.body) {
      if (auto var = dynamic_cast<const self::VarDeclaration *>(member.get())) {
        members.push_back(getType(var->getDecltype()));
      } else if (auto fun = dynamic_cast<const self::FunBase *>(member.get())) {
        functions.push_back(fun);
      }
    }
    members.shrink_to_fit();
    functions.shrink_to_fit();
    auto type = llvm::StructType::get(llvm, members);
    struct_map.insert({&str, type});
    for (const auto fun : functions) {
      generateFun(*fun);
    }
  }

  void emitLocation(Pos pos, llvm::IRBuilder<> &builder) {
    if (auto back = sp())
      builder.SetCurrentDebugLocation(
          llvm::DILocation::get(llvm, pos.line, pos.col, back));
  }

  void emitLocation(const ExprBase &expr, llvm::IRBuilder<> &builder) {
    emitLocation(expr.pos, builder);
  }

  unsigned int getDwarf(self::TypeRef t) {
    if (t.is_ref == self::RefTypes::ref) {
      return llvm::dwarf::DW_ATE_address;
    }
    if (t == context.i64_t) {
      return llvm::dwarf::DW_ATE_signed;
    }
    if (t == context.u64_t) {
      return llvm::dwarf::DW_ATE_unsigned;
    }
    if (t == context.void_t) {
      // todo maybe figure something else out???
      return 0;
    }
    if (t == context.bool_t) {
      return llvm::dwarf::DW_ATE_boolean;
    }
    if (t == context.str_t) {
      return llvm::dwarf::DW_ATE_ASCII;
    }

    throw std::runtime_error("non ints not implemented right now");
  }

  unsigned int getDwarf(const self::VarDeclaration &var) {
    return getDwarf(var.getDecltype());
  }

  llvm::DISubroutineType *createFunDebugType(const self::FunBase &fun) {
    llvm::SmallVector<llvm::Metadata *> fun_type;
    fun_type.push_back(getDIType(fun.return_type));
    forEachArg(fun, [&, this](const std::unique_ptr<self::VarDeclaration> &a) {
      fun_type.push_back(getDIType(a->getType()));
    });
    return di.createSubroutineType(di.getOrCreateTypeArray(fun_type));
  }

  llvm::DIType *getDIType(self::TypeRef t) {
    if (auto *n = dynamic_cast<const StructDef *>(&t.ptr)) {
      auto layout = module.getDataLayout().getStructLayout(
          llvm::cast<llvm::StructType>(struct_map.at(n)));
      // assumes 8 bit bytes
      auto alignment = layout->getAlignment().value() * 8;
      llvm::DIBuilder *d;
      std::vector<llvm::Metadata *> elements;
      elements.reserve(n->body.size());
      for (auto &member : n->body) {
        if (auto var =
                dynamic_cast<const self::VarDeclaration *>(member.get())) {
          elements.push_back(getDIType(var->getDecltype()));
        }
      }
      elements.shrink_to_fit();
      return str_ditype;
    } else if (t.ptr == context.str_t) {
      if (t.is_ref == self::RefTypes::value) {
        return str_ditype;
      } else {
        auto data_layout = module.getDataLayout();
        auto ptrbits = data_layout.getPointerSizeInBits();
        return di.createPointerType(str_ditype, ptrbits);
      }
    } else if (t.ptr == context.void_t) {
      return nullptr;
    } else {
      return di.createBasicType(t.ptr.getTypename(), getBitsize(t),
                                getDwarf(t));
    }
  }

  llvm::Constant *getZeroed(self::TypeRef t) {
    auto null_ptr =
        llvm::ConstantPointerNull::get(llvm::PointerType::get(llvm, 0));
    auto zero = llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm), 0);
    if (t.is_ref == self::RefTypes::ref) {
      return null_ptr;
    }
    if (t == context.i64_t) {
      return zero;
    }
    if (t == context.u64_t) {
      return zero;
    }
    if (t == context.bool_t) {
      return llvm::ConstantInt::get(llvm::Type::getInt1Ty(llvm), 0);
    }
    if (t == context.str_t) {
      llvm::ConstantStruct::get(str_type, {null_ptr, zero});
    }

    throw std::runtime_error("non ints not implemented right now");
  }

  size_t getBitsize(TypeRef t) {
    return module.getDataLayout().getTypeAllocSizeInBits(getType(t));
  }
};
void init() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();
}
llvm::TargetMachine *config_module(llvm::Module &module) {
  init();
  std::string triple_error;
  auto target_triple = llvm::sys::getDefaultTargetTriple();
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, triple_error);
  if (!target) {
    llvm::errs() << triple_error;
    throw std::runtime_error("Failure to find target.");
  }
  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto RM = llvm::Optional<llvm::Reloc::Model>();
  auto target_machine =
      target->createTargetMachine(target_triple, CPU, Features, opt, RM);
  module.setTargetTriple(target_triple);
  module.setDataLayout(target_machine->createDataLayout());
  return target_machine;
}
} // namespace

namespace self {
// must be returned by unique_ptr because module cannot be moved or copied

std::pair<std::unique_ptr<llvm::Module>, std::unique_ptr<llvm::DIBuilder>>
codegen(const self::Module &ast, Context &c, llvm::LLVMContext &llvm,
        std::filesystem::path file) {
  auto module = std::make_unique<llvm::Module>(file.string(), llvm);
  auto n = config_module(*module);
  // module->setDataLayout();
  module->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                        llvm::DEBUG_METADATA_VERSION);

  auto di = std::make_unique<llvm::DIBuilder>(*module);
  auto g = Generator(c, llvm, *module, *di, file, ast.struct_list);
  for (auto &expr : ast.ast) {
    if (auto *function = dynamic_cast<self::FunBase *>(expr.get())) {
      g.generateFun(*function);
    } else if (auto *assign = dynamic_cast<self::FunctionCall *>(expr.get())) {
      if (auto *global =
              dynamic_cast<self::VarDeclaration *>(assign->lhs.get())) {
        g.generateGlobal(*global);
      }
    } else {
      throw std::runtime_error("unimplemented");
    }
  }
  di->finalize();
  if (llvm::verifyModule(*module, &llvm::errs())) {
    std::exit(2);
  }

  return {std::move(module), std::move(di)};
}
} // namespace self