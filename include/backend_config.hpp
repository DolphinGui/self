#pragma once
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace self {
struct VarDeclaration;
struct ExpressionTree;
void compile(llvm::Module &, llvm::raw_fd_ostream &);

llvm::Type *getType(const VarDeclaration &);
std::unique_ptr<llvm::Module> codegen(const self::ExpressionTree &ast);
} // namespace self