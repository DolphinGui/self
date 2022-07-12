#pragma once
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>

namespace selflang {
struct var_decl;
struct expression_tree;
void compile(llvm::Module &, llvm::raw_fd_ostream &);

llvm::Type *getType(const var_decl &);
std::unique_ptr<llvm::Module> codegen(const selflang::expression_tree &ast);
} // namespace selflang