#ifndef C_GENERATOR_H_
#define C_GENERATOR_H_

#include "ast.h"
#include "common.h"

typedef struct c_generator c_generator;

typedef void c_generator_stmt_function(c_generator* generator, stmt* ast_node);
typedef void c_generator_expr_function(c_generator* generator, expr* ast_node);

struct c_generator {
    FILE*    output_file;

    // the indentation
    int32_t indent;
    // cache the indent
    char indent_buffer[1024];
};

c_generator c_generator_create(const char* to_write);
void        c_generator_clear(c_generator* generator);

void emit_c_literal_expr(c_generator* generator, expr* ast_node);
void emit_c_func_call_expr(c_generator* generator, expr* ast_node);
void emit_c_lvalue_expr(c_generator* generator, expr* ast_node);
void emit_c_binary_op_expr(c_generator* generator, expr* ast_node);

void emit_c_if_stmt(c_generator* generator, stmt* ast_node);
void emit_c_while_stmt(c_generator* generator, stmt* ast_node);
void emit_c_do_stmt(c_generator* generator, stmt* ast_node);
void emit_c_break_stmt(c_generator* generator, stmt* ast_node);
void emit_c_continue_stmt(c_generator* generator, stmt* ast_node);
void emit_c_return_stmt(c_generator* generator, stmt* ast_node);
void emit_c_block_stmt(c_generator* generator, stmt* ast_node);
void emit_c_var_assign_stmt(c_generator* generator, stmt* ast_node);
void emit_c_var_definition_stmt(c_generator* generator, stmt* ast_node);
void emit_c_expr_stmt(c_generator* generator, stmt* ast_node);

void emit_c_func_decl(c_generator* generator, func_decl* ast_func);
void emit_c_top_level(c_generator* generator, top_level* ast_top);
void emit_c_module(c_generator* generator, module* ast_module);

#endif // !C_GENERATOR_H_
