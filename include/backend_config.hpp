#pragma once
#include "builtins.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace self {
class VarDeclaration;
struct ExprTree;
void compile(llvm::Module &, llvm::raw_fd_ostream &);

llvm::Type *getType(const VarDeclaration &);
std::unique_ptr<llvm::Module> codegen(const self::ExprTree &ast,
                                      Context &c, llvm::LLVMContext& context);
} // namespace self