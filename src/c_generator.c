#include "c_generator.h"

static c_generator_expr_function* const c_generator_epxr_functions_table[EXPR_TYPES_COUNT] = {
    emit_c_literal_expr,
    emit_c_func_call_expr,
    emit_c_lvalue_expr,
    emit_c_binary_op_expr
};

static c_generator_stmt_function* const c_generator_stmt_functions_table[STMT_TYPES_COUNT] = {
    emit_c_if_stmt,
    emit_c_while_stmt,
    emit_c_do_stmt,
    emit_c_break_stmt,
    emit_c_continue_stmt,
    emit_c_return_stmt,
    emit_c_block_stmt,
    emit_c_var_assign_stmt,
    emit_c_var_definition_stmt,
    emit_c_expr_stmt,
};

static void compute_indent(c_generator* generator) {
    char* ident_buffer        = generator->indent_buffer;
    uint32_t current_ident    = generator->indent;
    uint32_t indent_buffer_it = 0;

    for (indent_buffer_it = 0;
         indent_buffer_it < current_ident;
         indent_buffer_it++)
    {
        ident_buffer[indent_buffer_it] = '\t';
    }
    ident_buffer[indent_buffer_it] = '\0';
}

static inline const char* get_indent(c_generator* generator) {
    return (const char*) generator->indent_buffer;
}

static inline void enter_scope(c_generator* generator) {
    fprintf(generator->output_file, "{\n");

    generator->indent += 1;

    compute_indent(generator);
}

static inline void exit_scope(c_generator* generator) {
    generator->indent -= 1;
    if (generator->indent < 0)
        generator->indent = 0;

    compute_indent(generator);

    fprintf(generator->output_file, "%s}\n", get_indent(generator));
}

const char* convert_var_type_to_string(var_type type) {
    switch (type) {
        case U8_VAR:   return "u8";
        case U16_VAR:  return "u16";
        case U32_VAR:  return "u32";
        case U64_VAR:  return "u64";
        case I8_VAR:   return "i8";
        case I16_VAR:  return "i16";
        case I32_VAR:  return "i32";
        case I64_VAR:  return "i64";
        case F32_VAR:  return "f32";
        case F64_VAR:  return "f64";
        case BOOL_VAR: return "bool";
        case STR_VAR:  return "char*";
        case PTR_VAR:  return "*";
        case VOID_VAR: return "void";
        default:       return "none";
    }
}

c_generator c_generator_create(const char* to_write) {
    c_generator generator = {};

    generator.indent = 0;
    memset(&generator.indent_buffer, '\0', 1024);

    if (!(generator.output_file = fopen(to_write, "w"))) {
        fprintf(stderr, "unable to open file '%s'.", to_write);
        exit(1);
    }

    return generator;
}

void c_generator_clear(c_generator* generator) {
    fclose(generator->output_file);
}

void emit_c_literal_expr(c_generator *generator, expr *ast_node) {
}

void emit_c_func_call_expr(c_generator *generator, expr *ast_node) {
}

void emit_c_lvalue_expr(c_generator *generator, expr *ast_node) {
}

void emit_c_binary_op_expr(c_generator *generator, expr *ast_node) {
}

void emit_c_if_stmt(c_generator* generator, stmt* ast_node) {
   assert(generator);
   assert(ast_node);

   if_stmt* ast_if = (if_stmt*) &ast_node->as.iff;

   assert(ast_if);

   {
       fprintf(generator->output_file, "%sif (", get_indent(generator));

       stmt ast_expr = {};
       ast_expr.type     = EXPRESSION_STMT;
       ast_expr.as.exprr = ast_if->condition;

       emit_c_expr_stmt(generator, &ast_expr);

       fprintf(generator->output_file, ") ");
   }

   {
       stmt ast_block = {};
       ast_block.type     = BLOCK_STMT;
       ast_block.as.block = ast_if->then_body;

       emit_c_block_stmt(generator, &ast_block);
   }

   {
       if (ast_if->has_else) {
           fprintf(generator->output_file, "%selse ", get_indent(generator));

           stmt ast_block = {};
           ast_block.type     = BLOCK_STMT;
           ast_block.as.block = ast_if->else_body;
           emit_c_block_stmt(generator, &ast_block);
       }
   }
}

void emit_c_while_stmt(c_generator *generator, stmt *ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == WHILE_STMT);

    while_stmt* ast_while = (while_stmt*) &ast_node->as.whilee;

    assert(ast_while);

    {
        fprintf(generator->output_file, "%swhile (", get_indent(generator));

        stmt ast_expr = {};
        ast_expr.type     = EXPRESSION_STMT;
        ast_expr.as.exprr = ast_while->condition;

        emit_c_expr_stmt(generator, &ast_expr);

        fprintf(generator->output_file, ") ");
    }

    {
       stmt ast_block = {};
       ast_block.type     = BLOCK_STMT;
       ast_block.as.block = ast_while->body;

       emit_c_block_stmt(generator, &ast_block);
    }
}

void emit_c_do_stmt(c_generator *generator, stmt *ast_node) {
    assert(generator);
    assert(ast_node);

    (void) generator;
    (void) ast_node;
}

void emit_c_break_stmt(c_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node);

    break_stmt* ast_break = (break_stmt*) &ast_node->as.brk;

    assert(ast_break);

    fprintf(generator->output_file, "%sbreak;\n", get_indent(generator));
}

void emit_c_continue_stmt(c_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node);

    continue_stmt* ast_continue = (continue_stmt*) &ast_node->as.cont;

    assert(ast_continue);

    fprintf(generator->output_file, "%scontinue;\n", get_indent(generator));
}

void emit_c_return_stmt(c_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node);

    return_stmt* ast_return = (return_stmt*) &ast_node->as.returnn;

    assert(ast_return);

    if (ast_return->has_result) {
        fprintf(generator->output_file, "%sreturn ", get_indent(generator));

        stmt ast_expr = {};
        ast_expr.type     = EXPRESSION_STMT;
        ast_expr.as.exprr = ast_return->result;

        emit_c_expr_stmt(generator, &ast_expr);
    } else {
        fprintf(generator->output_file, "%sreturn;", get_indent(generator));
    }
}

void emit_c_block_stmt(c_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == BLOCK_STMT);

    block_stmt* ast_block = &ast_node->as.block;

    assert(ast_block);

    enter_scope(generator);
    {
        for (list_it it = ast_block->stmts->front;
             it != NULL;
             it = it->next)
        {
            stmt* ast_stmt = (stmt*) it->data;

            assert(ast_stmt->type >= IF_STMT &&
                   ast_stmt->type <= EXPRESSION_STMT);

            c_generator_stmt_functions_table[ast_stmt->type](generator, ast_stmt);
        }
    }
    exit_scope(generator);
}

void emit_c_var_assign_stmt(c_generator *generator, stmt *ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == VAR_ASSIGN_STMT);

    var_assign* ast_var_assign = (var_assign*) &ast_node->as.var_assi;

    assert(ast_var_assign);

    fprintf(generator->output_file,
            "%s%s = ",
            get_indent(generator),
            ast_var_assign->name.data);

    {
        stmt ast_expr = {};
        ast_expr.type     = EXPRESSION_STMT;
        ast_expr.as.exprr = ast_var_assign->value;

        emit_c_expr_stmt(generator, &ast_expr);
    }

    fprintf(generator->output_file, ";\n");
}

void emit_c_var_definition_stmt(c_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == VAR_DEFINITION_STMT);

    var_def* ast_var_def = (var_def*) &ast_node->as.var_defi;

    assert(ast_var_def);

    fprintf(generator->output_file,
            "%s%s ",
            get_indent(generator),
            convert_var_type_to_string(ast_var_def->type));

    if (ast_var_def->is_initialized) {
        var_assign ast_assign = {};
        ast_assign.loc   = ast_var_def->loc;
        ast_assign.name  = ast_var_def->name;
        ast_assign.value = ast_var_def->rhs;

        stmt ast_var_assign = {};
        ast_var_assign.type        = VAR_ASSIGN_STMT;
        ast_var_assign.as.var_assi = ast_assign;

        emit_c_var_assign_stmt(generator, &ast_var_assign);
    } else {
        fprintf(generator->output_file, "%s;\n", ast_var_def->name.data);
    }
}

void emit_c_expr_stmt(c_generator *generator, stmt *ast_node) {

}

void emit_c_func_decl(c_generator* generator, func_decl* ast_func) {
    assert(generator);
    assert(ast_func);

    {
        fprintf(generator->output_file, "%s %s(", convert_var_type_to_string(ast_func->return_type), ast_func->name.data);
        for (list_it it = ast_func->parameters->front;
             it != NULL;
             it = it->next)
        {
            func_param* param = (func_param*) it->data;
            if (it->next != NULL) {
                fprintf(generator->output_file, "%s %s, ", convert_var_type_to_string(param->type), param->name.data);
            } else {
                fprintf(generator->output_file, "%s %s", convert_var_type_to_string(param->type), param->name.data);
            }
        }
        fprintf(generator->output_file, ") ");
    }

    {
        stmt ast_block = {};
        ast_block.type     = BLOCK_STMT;
        ast_block.as.block = ast_func->body;

        emit_c_block_stmt(generator, &ast_block);
    }
}

void emit_c_top_level(c_generator *generator, top_level *ast_top) {
    assert(generator);
    assert(ast_top);
    assert(ast_top->type == VAR_DEF_TL ||
           ast_top->type == FUN_DECL_TL);

    switch (ast_top->type) {
        case VAR_DEF_TL: {
            stmt var_defi_stmt = {};
            var_defi_stmt.type        = VAR_DEFINITION_STMT;
            var_defi_stmt.as.var_defi = ast_top->as.var;

            emit_c_var_definition_stmt(generator, &var_defi_stmt);
        } break;

        case FUN_DECL_TL: {
            emit_c_func_decl(generator, &ast_top->as.func);
        } break;

        default: {
            assert(false && "should not be here.");
        } break;
    }
}

static void module_for_each(list_it it, void* extra) {
    top_level* ast_top     = (top_level*) it->data;
    c_generator* generator = (c_generator*) extra;

    emit_c_top_level(generator, ast_top);
}

void emit_c_module(c_generator* generator, module* ast_module) {
    assert(generator);
    assert(ast_module);

    fprintf(generator->output_file, "#include <stdint.h>\n");
    fprintf(generator->output_file, "#include <stdbool.h>\n\n");
    fprintf(generator->output_file, "typedef uint8_t u8;\n");
    fprintf(generator->output_file, "typedef uint16_t u16;\n");
    fprintf(generator->output_file, "typedef uint32_t u32;\n");
    fprintf(generator->output_file, "typedef uint64_t u64;\n");
    fprintf(generator->output_file, "typedef int8_t i8;\n");
    fprintf(generator->output_file, "typedef int16_t i16;\n");
    fprintf(generator->output_file, "typedef int32_t i32;\n");
    fprintf(generator->output_file, "typedef int64_t i64;\n");
    fprintf(generator->output_file, "typedef float f32;\n");
    fprintf(generator->output_file, "typedef double f64;\n\n");

    list_for_each(ast_module->modules, module_for_each, generator);
}
