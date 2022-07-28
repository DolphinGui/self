#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <system_error>

using namespace llvm;

void init() {
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();
}
TargetMachine *config_module(Module &module) {
  init();
  std::string triple_error;
  auto target_triple = sys::getDefaultTargetTriple();
  auto target = TargetRegistry::lookupTarget(target_triple, triple_error);
  if (!target) {
    errs() << triple_error;
    throw std::runtime_error("Failure to find target.");
  }
  auto CPU = "generic";
  auto Features = "";

  TargetOptions opt;
  auto RM = Optional<Reloc::Model>();
  auto target_machine =
      target->createTargetMachine(target_triple, CPU, Features, opt, RM);
  module.setTargetTriple(target_triple);
  module.setDataLayout(target_machine->createDataLayout());
  return target_machine;
}

int main() {
  LLVMContext c;
  c.enableOpaquePointers();
  auto m = Module("this is a module name I guess", c);
  auto int_t = Type::getInt64Ty(c);
  auto ptr_t = PointerType::get(c, 0);
  auto printer = Function::Create(FunctionType::get(int_t, {ptr_t}, false),
                                  GlobalValue::ExternalLinkage, "printnum", m);
  auto main = Function::Create(FunctionType::get(int_t, false),
                               GlobalValue::ExternalLinkage, "main", m);
  auto block = BasicBlock::Create(c, "entry", main);
  auto builder = IRBuilder<>(c);
  builder.SetInsertPoint(block);
  auto a = builder.CreateAlloca(int_t);
  auto int_lit = ConstantInt::get(int_t, APInt(64, 123, true));
  builder.CreateStore(int_lit, a);
  auto b = builder.CreateAlloca(PointerType::get(c, 0));
  builder.CreateStore(a, b);
  builder.CreateCall(printer, a);
  auto loadb = builder.CreateLoad(ptr_t, b);
  builder.CreateCall(printer, loadb);
  builder.CreateRet(int_lit);
  m.print(outs(), nullptr);
  legacy::PassManager pass;
  std::error_code e;
  auto dest = raw_fd_ostream("a.o", e);
  auto target = config_module(m);
  if (target->addPassesToEmitFile(pass, dest, nullptr,
                                  CodeGenFileType::CGFT_ObjectFile, false)) {
    errs() << "TargetMachine can't emit a file of this type\n";
    throw std::runtime_error("");
  }

  pass.add(createInstructionCombiningPass());
  pass.add(createReassociatePass());
  pass.add(createGVNPass());
  pass.add(createCFGSimplificationPass());
  pass.run(m);
  dest.flush();
}