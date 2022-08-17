#ifndef LLVM_GENERATOR_H_
#define LLVM_GENERATOR_H_

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include "ast.h"
#include "build_in.h"
#include "symbol_table.h"

#define UNWRAP(type, object) (type) *((type*) object)

typedef struct build_in_function build_in_function;
typedef struct bc_patch_info     bc_patch_info;
typedef struct llvm_symbol       llvm_symbol;
typedef struct llvm_generator    llvm_generator;

typedef LLVMValueRef llvm_generator_stmt_function(llvm_generator* generator, stmt* ast_node);
typedef LLVMValueRef llvm_generator_expr_function(llvm_generator* generator, expr* ast_node);

struct build_in_function {
    LLVMValueRef  function;
    const char*   name;
    void*         address;
};

struct bc_patch_info {
    LLVMValueRef       instr;
};

struct llvm_symbol {
    int32_t      scope;
    LLVMValueRef value;
    LLVMTypeRef  type;
};

struct llvm_generator {
    LLVMContextRef context;
    LLVMModuleRef  module;
    LLVMBuilderRef ir_builder;

    // How many errors so far?
    int32_t errors;

    // 0 == Global Scope
    int32_t scope;

    // 0 == break/continue is out of a while/do loop and is
    // not allowed
    int32_t bc_allowed;

    // llvm_generator phase has it own memory arena
    memory_arena* arena;

    // Symbol table
    symbol_table* symbols;

    // Keep a list of blocks
    list* blocks;

    // Keep track of the defered patch info for continue and
    // break instructions
    list* break_list;
    list* continue_list;

    build_in_function build_in_functions[BUILD_IN_FUNCTIONS_COUNT];
    LLVMTypeRef       types_to_llvm[VAR_TYPES_COUNT];
};

llvm_generator llvm_generator_create(const char* module_name);
void           llvm_generator_clear(llvm_generator* generator);
LLVMValueRef   llvm_generator_report_error(llvm_generator* generator, location* loc, const char* format, ...);

// Take the AST and generate LLVM IR
LLVMValueRef emit_llvm_literal_expr(llvm_generator* generator, expr* ast_node);
LLVMValueRef emit_llvm_func_call_expr(llvm_generator* generator, expr* ast_node);
LLVMValueRef emit_llvm_lvalue_expr(llvm_generator* generator, expr* ast_node);
LLVMValueRef emit_llvm_binary_op_expr(llvm_generator* generator, expr* ast_node);

LLVMValueRef emit_llvm_if_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_while_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_do_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_break_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_continue_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_return_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_block_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_var_assign_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_var_definition_stmt(llvm_generator* generator, stmt* ast_node);
LLVMValueRef emit_llvm_expr_stmt(llvm_generator* generator, stmt* ast_node);

LLVMValueRef emit_llvm_func_decl(llvm_generator* generator, func_decl* ast_func);
void emit_llvm_top_level(llvm_generator* generator, top_level* ast_top);
void emit_llvm_module(llvm_generator* generator, module* ast_mod);

#endif // !LLVM_GENERATOR_H_
