#pragma once
#include "builtins.hpp"
#include <filesystem>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace self {
class VarDeclaration;
struct ExprTree;
void compile(llvm::Module &, llvm::raw_pwrite_stream &file);

llvm::Type *getType(const VarDeclaration &);
std::unique_ptr<llvm::Module> codegen(const self::ExprTree &ast, Context &c,
                                      llvm::LLVMContext &context);
} // namespace self