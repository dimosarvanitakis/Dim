#ifndef CODEGEN_H_
#define CODEGEN_H_

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "ast.h"
#include "build_in.h"
#include "symbol_table.h"

typedef struct build_in_function build_in_function;
typedef struct bc_patch_info     bc_patch_info;
typedef struct codegen_symbol    codegen_symbol;
typedef struct codegen           codegen;

struct build_in_function {
    LLVMValueRef  function;
    const char*   name;
    void*         address;
};

struct bc_patch_info {
    LLVMValueRef       instr;
    LLVMBasicBlockRef* parent;
};

struct codegen_symbol {
    int32_t      scope;
    bool         is_function;
    LLVMValueRef value;
    LLVMTypeRef  type;
};

struct codegen {
    LLVMContextRef context;
    LLVMModuleRef  module;
    LLVMBuilderRef ir_builder;

    // How many errors so far?
    int32_t errors;

    // codegen phase has it own memory arena
    arena mem;

    // Symbol table
    symbol_table symbols;

    // Keep a list of blocks
    list blocks;

    // Keep track of the defered patch info for continue and
    // break instructions
    list break_list;
    list continue_list;

    build_in_function build_in_functions[BUILD_IN_FUNCTIONS_COUNT];
    LLVMTypeRef       types_to_llvm[VAR_TYPES_COUNT];
};

codegen      codegen_create(const char* module_name);
void         codegen_clear(codegen* code);
LLVMValueRef codegen_report_error(codegen* code, location* loc, const char* format, ...);

// Take the AST and generate LLVM IR
LLVMValueRef codegen_generate_literal_expr(codegen* code, expr* ast_node);
LLVMValueRef codegen_generate_func_call_expr(codegen* code, expr* ast_node);
LLVMValueRef codegen_generate_lvalue_expr(codegen* code, expr* ast_node);
LLVMValueRef codegen_generate_binary_op_expr(codegen* code, expr* ast_node);

LLVMValueRef codegen_generate_if_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_while_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_do_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_break_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_continue_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_return_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_block_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_var_assign_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_var_definition_stmt(codegen* code, stmt* ast_node);
LLVMValueRef codegen_generate_expr_stmt(codegen* code, stmt* ast_node);

LLVMValueRef codegen_generate_func_decl(codegen* code, func_decl* ast_func);
void codegen_generate_top_level(codegen* code, top_level* ast_top);
void codegen_generate_module(codegen* code, module* ast_mod);

#endif // CODEGEN_H_
