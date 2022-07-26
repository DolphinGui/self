#include "backend_config.hpp"

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <stdexcept>
#include <system_error>

namespace {
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

void use_passes(llvm::Module &module, llvm::TargetMachine *target,
                llvm::raw_pwrite_stream &dest) {
  llvm::legacy::PassManager pass;

  if (target->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile)) {
    llvm::errs() << "TargetMachine can't emit a file of this type\n";
    throw std::runtime_error("");
  }

  pass.add(llvm::createInstructionCombiningPass());
  pass.add(llvm::createReassociatePass());
  pass.add(llvm::createGVNPass());
  pass.add(llvm::createCFGSimplificationPass());
  pass.run(module);
  dest.flush();
}
} // namespace
namespace self {
void compile(llvm::Module &module, llvm::raw_pwrite_stream &file) {
  auto target = config_module(module);
  use_passes(module, target, file);
}
} // namespace self