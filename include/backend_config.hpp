#pragma once
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

namespace selflang {
void compile(llvm::Module &, llvm::raw_fd_ostream &);
}