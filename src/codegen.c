#include "codegen.h"

#define UNWRAP(type, object) (type) *((type*) object)

typedef LLVMValueRef codegen_stmt_function(codegen* code, stmt* ast_node);
typedef LLVMValueRef codegen_expr_function(codegen* code, expr* ast_node);

// 0 == Global Scope
static int32_t scope = 0;

//Dispatch table
static codegen_stmt_function* const codegen_stmt_functions_table[STMT_TYPES_COUNT] = {
    codegen_generate_if_stmt,
    codegen_generate_while_stmt,
    codegen_generate_do_stmt,
    codegen_generate_break_stmt,
    codegen_generate_continue_stmt,
    codegen_generate_return_stmt,
    codegen_generate_block_stmt,
    codegen_generate_var_assign_stmt,
    codegen_generate_var_definition_stmt,
    codegen_generate_expr_stmt,
};

static codegen_expr_function* const codegen_epxr_functions_table[EXPR_TYPES_COUNT] = {
    codegen_generate_literal_expr,
    codegen_generate_func_call_expr,
    codegen_generate_lvalue_expr,
    codegen_generate_binary_op_expr
};

static const char* binary_op_to_str_table[BINARY_OP_TYPES_COUNT] = {
    [OR_BINARY_OP]            = "||",
    [AND_BINARY_OP]           = "&&",
    [EQUALS_BINARY_OP]        = "==",
    [NOT_EQUALS_BINARY_OP]    = "!=",
    [GREATER_BINARY_OP]       = ">",
    [GREATER_EQUAL_BINARY_OP] = ">=",
    [LESS_BINARY_OP]          = "<",
    [LESS_EQUAL_BINARY_OP]    = "<=",
    [PLUS_BINARY_OP]          = "+",
    [MINUS_BINARY_OP]         = "-",
    [MULTI_BINARY_OP]         = "*",
    [DIV_BINARY_OP]           = "/",
    [MOD_BINARY_OP]           = "%"
};

static inline void enter_scope() {
    scope++;
}

static inline void exit_scope() {
    scope--;
}

static inline int32_t get_current_scope() {
    return scope;
}

static inline void push_new_block(codegen* code) {
    LLVMBasicBlockRef new_block = LLVMCreateBasicBlockInContext(code->context,
                                                                "entry");
    list_push_back(code->mem, &code->blocks, &new_block);
}

static inline void push_existing_block(codegen* code, LLVMBasicBlockRef existing_block) {
    list_push_back(code->mem, &code->blocks, &existing_block);
}

static inline void pop_block(codegen* code) {
    list_pop_back(&code->blocks);
}

static inline LLVMBasicBlockRef get_current_block(codegen* code) {
    list_it current_block = list_back(&code->blocks);

    return UNWRAP(LLVMBasicBlockRef, current_block->data);
}

static inline void create_main_block(codegen* code) {
    // We need to create a global function for the builder to start
    // generating ir in case we have global variables.
    LLVMTypeRef main_params[]    = {};
    LLVMTypeRef main_func_type   = LLVMFunctionType(LLVMVoidTypeInContext(code->context), main_params, 0, false);
    LLVMValueRef main_func       = LLVMAddFunction(code->module, "global", main_func_type);
    LLVMBasicBlockRef main_block = LLVMAppendBasicBlockInContext(code->context, main_func, "entry");

    //LLVMPositionBuilderAtEnd(code->ir_builder, main_block);

    push_existing_block(code, main_block);
}

static bool is_build_in_function(codegen* code, const char* name) {
    for (int32_t func = 0; func < BUILD_IN_FUNCTIONS_COUNT; func++) {
        if (!strcmp(code->build_in_functions[func].name, name)) {
            return true;
        }
    }
    return false;
}

static void append_build_in_function(codegen* code,
                                     size_t index,
                                     LLVMValueRef func_value,
                                     LLVMTypeRef  func_type,
                                     const char*  func_name,
                                     void*        func_address)
{
    code->build_in_functions[index].function = func_value;
    code->build_in_functions[index].name     = func_name;
    code->build_in_functions[index].address  = func_address;

    codegen_symbol* func_symbol = (codegen_symbol*) arena_allocate(code->mem, sizeof(codegen_symbol));
    func_symbol->scope       = get_current_scope();
    func_symbol->is_function = true;

    func_symbol->value = arena_allocate(code->mem, sizeof(LLVMValueRef));
    memcpy(&func_symbol->value, &func_value, sizeof(LLVMValueRef));

    func_symbol->type  = arena_allocate(code->mem, sizeof(LLVMTypeRef));
    memcpy(&func_symbol->type, &func_type, sizeof(LLVMTypeRef));

    if (symbol_table_put(code->mem, &code->symbols, func_name, (void*) func_symbol) != SYM_SUCCESS) {
        fprintf(stderr, "Unable to create build in function '%s'.\n", func_name);
        exit(1);
    }
}

static void create_build_in_functions(codegen* code) {
    // print
    {
        LLVMTypeRef print_param_types[] = { code->types_to_llvm[STR_VAR] };
        LLVMTypeRef print_type          = LLVMFunctionType(LLVMVoidTypeInContext(code->context),
                                                       print_param_types, 1, 1);
        LLVMValueRef print_function     = LLVMAddFunction(code->module, "print", print_type);

        append_build_in_function(code, 0, print_function, print_type, "print", print);
    }

    // inputc
    {
        LLVMTypeRef inputc_param_types[] = {};
        LLVMTypeRef inputc_type          = LLVMFunctionType(LLVMInt8TypeInContext(code->context),
                                                        inputc_param_types, 0, 0);
        LLVMValueRef inputc_function     = LLVMAddFunction(code->module, "inputc", inputc_type);

        append_build_in_function(code, 1, inputc_function, inputc_type, "inputc", inputc);
    }
}

codegen codegen_create(arena* mem, const char* module_name) {
    codegen code = {0};

    code.context    = LLVMContextCreate();
    code.module     = LLVMModuleCreateWithNameInContext(module_name, code.context);
    code.ir_builder = LLVMCreateBuilderInContext(code.context);

    code.mem     = mem;
    code.symbols = symbol_table_create(mem);
    code.blocks  = list_create(sizeof(LLVMBasicBlockRef*));

    code.errors  = 0;

    LLVMTypeRef types_to_llvm[VAR_TYPES_COUNT] = {
        LLVMInt8TypeInContext(code.context),
        LLVMInt16TypeInContext(code.context),
        LLVMInt32TypeInContext(code.context),
        LLVMInt64TypeInContext(code.context),
        LLVMInt8TypeInContext(code.context),
        LLVMInt16TypeInContext(code.context),
        LLVMInt32TypeInContext(code.context),
        LLVMInt64TypeInContext(code.context),
        LLVMFloatTypeInContext(code.context),
        LLVMDoubleTypeInContext(code.context),
        LLVMInt32TypeInContext(code.context),
        LLVMPointerType(LLVMInt8TypeInContext(code.context), 0), // string
        LLVMPointerType(LLVMInt8TypeInContext(code.context), 0), // ptr
        LLVMVoidTypeInContext(code.context), // void
        LLVMVoidTypeInContext(code.context)  // none
    };
    memcpy(code.types_to_llvm, types_to_llvm, VAR_TYPES_COUNT * sizeof(LLVMTypeRef));

    create_main_block(&code);
    create_build_in_functions(&code);

    return code;
}

void codegen_clear(codegen* code) {
    LLVMDisposeModule(code->module);
    LLVMDisposeBuilder(code->ir_builder);
    LLVMContextDispose(code->context);
}

LLVMValueRef codegen_report_error(codegen* code, location* loc, const char* format, ...) {
    fprintf(stderr, "%s:%u:%u ~ code generatin error: ", loc->file, loc->line, loc->column);

    va_list vargs;
    va_start(vargs, format);
    vfprintf(stderr, format, vargs);
    fprintf(stderr, "\n");
    va_end(vargs);

    code->errors++;

    return NULL;
}

LLVMValueRef codegen_generate_literal_expr(codegen* code, expr* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == LITERAL_EXPR);

    literal_expr* ast_literal = &ast_node->as.literal;

    assert(ast_literal                      &&
           ast_literal->type >= INTEGER_LIT &&
           ast_literal->type <= STRING_LIT);

    switch(ast_literal->type) {
        case INTEGER_LIT: {
            LLVMTypeRef  type  = LLVMInt32TypeInContext(code->context);
            LLVMValueRef value = LLVMConstInt(type, ast_literal->as.int_val, 1);

            return value;
        } break;

        case DOUBLE_LIT: {
             LLVMTypeRef  type  = LLVMDoubleTypeInContext(code->context);
             LLVMValueRef value = LLVMConstReal(type, ast_literal->as.double_val);

             return value;
        } break;

        case BOOL_LIT: {
            LLVMTypeRef  type = LLVMInt1TypeInContext(code->context);
            LLVMValueRef value;
            if (ast_literal->as.bool_val)
                value = LLVMConstInt(type, 1, 0);
            else
                value = LLVMConstInt(type, 0, 0);

            return value;
        } break;

        case STRING_LIT: {
            int32_t size = strlen(ast_literal->as.string_val);

            LLVMTypeRef g_str_type = LLVMArrayType(LLVMInt8TypeInContext(code->context), size + 1);
            LLVMValueRef g_str_val = LLVMAddGlobal(code->module, g_str_type, ".str");
            LLVMSetGlobalConstant(g_str_val, 1);
            LLVMSetAlignment(g_str_val, 1);
            LLVMSetLinkage(g_str_val, LLVMPrivateLinkage);
            LLVMSetUnnamedAddress(g_str_val, LLVMGlobalUnnamedAddr);

            LLVMValueRef c_str_val = LLVMConstStringInContext(code->context, ast_literal->as.string_val, size, 0);
            LLVMSetInitializer(g_str_val, c_str_val);

            LLVMValueRef zero_index = LLVMConstInt(LLVMInt64TypeInContext(code->context), 0, true);
            LLVMValueRef indexes[]  = {zero_index, zero_index};

            LLVMValueRef pointer = LLVMBuildInBoundsGEP2(code->ir_builder, g_str_type, g_str_val, indexes, 2, "");

            return pointer;
        } break;
    }
}

LLVMValueRef codegen_generate_func_call_expr(codegen* code, expr* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == FUNC_CALL_EXPR);

    func_call_expr* ast_func_call = &ast_node->as.func_call;

    assert(ast_func_call);

    LLVMValueRef callee_func = LLVMGetNamedFunction(code->module, ast_func_call->name.data);
    if (!callee_func) {
        return codegen_report_error(code, &ast_func_call->loc,
                                    "trying to call an unknown function with name `%s`.",
                                    ast_func_call->name.data);
    }

    uint32_t callee_parama_size = LLVMCountParams(callee_func);
    uint32_t caller_pass_args   = ast_func_call->arguments.length;
    if (callee_parama_size != caller_pass_args) {
        return codegen_report_error(code, &ast_func_call->loc,
                                    "trying to call function `%s` with less parameters (expected %u and got %u)",
                                    ast_func_call->name.data,
                                    callee_parama_size,
                                    caller_pass_args);
    }

    LLVMValueRef args[callee_parama_size];

    list_it args_it = ast_func_call->arguments.front;
    uint32_t arg_n  = 0;
    while (args_it != NULL && arg_n < callee_parama_size) {
        expr* arg_expr = (expr*) args_it->data;

        stmt arg_stmt = {0};
        arg_stmt.type = EXPRESSION_STMT;
        memcpy(&arg_stmt.as.exprr, arg_expr, sizeof(expr));

        LLVMValueRef arg_expr_value = codegen_generate_expr_stmt(code, &arg_stmt);

        args[arg_n] = arg_expr_value;

        args_it = args_it->next;
        arg_n   = arg_n + 1;
    }

    return LLVMBuildCall2(code->ir_builder,
                          LLVMGetReturnType(LLVMTypeOf(callee_func)),
                          callee_func,
                          args,
                          callee_parama_size,
                          "calltmp");
}

LLVMValueRef codegen_generate_lvalue_expr(codegen* code, expr* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == LVALUE_EXPR);

    lvalue_expr* ast_lvalue = &ast_node->as.lvalue;

    assert(ast_lvalue);

    codegen_symbol* id;
    if ((id = (codegen_symbol*) symbol_table_get(&code->symbols, ast_lvalue->name.data))) {
        LLVMTypeRef  type  = id->type;
        LLVMValueRef value = id->value;

        return LLVMBuildLoad2(code->ir_builder,
                              type,
                              value,
                              ast_lvalue->name.data);
    } else {
        return codegen_report_error(code, &ast_lvalue->loc,
                                    "undeclared variable `%s`.",
                                    ast_lvalue->name.data);
    }
}

static LLVMValueRef codegen_generate_binary_compare(codegen* code, binary_op_expr* ast_binary_op, LLVMValueRef lhs, LLVMValueRef rhs) {
    LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
    LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

    if (lhs_kind != LLVMFloatTypeKind  &&
        lhs_kind != LLVMDoubleTypeKind &&
        lhs_kind != LLVMIntegerTypeKind) {
        return codegen_report_error(code, &ast_binary_op->loc, "left hand side of compare expression is not a number.");
    }

    if (rhs_kind != LLVMFloatTypeKind  &&
        rhs_kind != LLVMDoubleTypeKind &&
        rhs_kind != LLVMIntegerTypeKind) {
        return codegen_report_error(code, &ast_binary_op->loc, "right hand side of compare expression is not a number.");
    }

    LLVMIntPredicate int_binary_op;
    LLVMRealPredicate real_binary_op;
    bool is_real = (rhs_kind == LLVMFloatTypeKind ||
                    rhs_kind == LLVMDoubleTypeKind) ? 1 : 0;

    switch(ast_binary_op->type) {
        case EQUALS_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealOEQ;
            } else {
                int_binary_op  = LLVMIntEQ;
            }
        } break;

        case NOT_EQUALS_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealONE;
            } else {
                int_binary_op  = LLVMIntNE;
            }
        } break;

        case GREATER_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealOGT;
            } else {
                int_binary_op  = LLVMIntSGT;
            }
        } break;

        case GREATER_EQUAL_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealOGE;
            } else {
                int_binary_op  = LLVMIntSGE;
            }
        } break;

        case LESS_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealOLT;
            } else {
                int_binary_op  = LLVMIntSLT;
            }
        } break;

        case LESS_EQUAL_BINARY_OP: {
            if (is_real) {
                real_binary_op = LLVMRealOLE;
            } else {
                int_binary_op  = LLVMIntSLE;
            }
        } break;

        default: {
            return codegen_report_error(code, &ast_binary_op->loc,
                                        "unknown comparison operator `%s`.\n",
                                        binary_op_to_str_table[ast_binary_op->type]);
        } break;
    }

    if (is_real) {
        return LLVMBuildFCmp(code->ir_builder, real_binary_op, lhs, rhs, "comptmp");
    } else {
        return LLVMBuildICmp(code->ir_builder, int_binary_op, lhs, rhs, "comptmp");
    }
}

static LLVMValueRef codegen_generate_binary_arithetic(codegen* code, binary_op_expr* ast_binary_op, LLVMValueRef lhs, LLVMValueRef rhs) {
    LLVMTypeKind kind = LLVMGetTypeKind(LLVMTypeOf(rhs));
    bool is_real      = (kind == LLVMFloatTypeKind ||
                         kind == LLVMDoubleTypeKind) ? 1 : 0;

    if ((ast_binary_op->type == OR_BINARY_OP ||
         ast_binary_op->type == AND_BINARY_OP) && is_real) {
        return codegen_report_error(code, &ast_binary_op->loc,
                                    "the binary operators '||' and '&&' does not support floating point arithetic.\n");
    }

    LLVMOpcode arith_opcode;
    switch(ast_binary_op->type) {
        case OR_BINARY_OP: {
            arith_opcode = LLVMOr;
        } break;
        case AND_BINARY_OP:{
            arith_opcode = LLVMAnd;
        } break;

        case PLUS_BINARY_OP: {
            arith_opcode = is_real ? LLVMFAdd : LLVMAdd;
        } break;

        case MINUS_BINARY_OP: {
            arith_opcode = is_real ? LLVMFSub : LLVMSub;
        } break;

        case MULTI_BINARY_OP: {
            arith_opcode = is_real ? LLVMFMul : LLVMMul;
        } break;

        case DIV_BINARY_OP: {
            arith_opcode = is_real ? LLVMFDiv : LLVMSDiv;
        } break;

        case MOD_BINARY_OP: {
            arith_opcode = is_real ? LLVMFRem : LLVMSRem;
        } break;

        // Handle the rest of the cases as error
        default: {
            return codegen_report_error(code, &ast_binary_op->loc,
                                        "unknown arithetic operator `%s`.\n",
                                        binary_op_to_str_table[ast_binary_op->type]);
        } break;
    }

    return LLVMBuildBinOp(code->ir_builder, arith_opcode, lhs, rhs, "arithtemp");
}

LLVMValueRef codegen_generate_binary_op_expr(codegen *code, expr *ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == BINARY_OP_EXPR);

    binary_op_expr* ast_binary_op = ast_node->as.binary_op;

    assert(ast_binary_op);

    stmt lhs_stmt = {0};
    lhs_stmt.type = EXPRESSION_STMT;
    memcpy(&lhs_stmt.as.exprr, &ast_binary_op->lhs, sizeof(expr));
    LLVMValueRef rhs_value = codegen_generate_expr_stmt(code, &lhs_stmt);

    stmt rhs_stmt = {0};
    rhs_stmt.type = EXPRESSION_STMT;
    memcpy(&rhs_stmt.as.exprr, &ast_binary_op->rhs, sizeof(expr));
    LLVMValueRef lhs_value = codegen_generate_expr_stmt(code, &rhs_stmt);

    if (!lhs_value || !rhs_value)
        return NULL;

    if (ast_binary_op->type >= EQUALS_BINARY_OP &&
        ast_binary_op->type <= LESS_BINARY_OP) {
        return codegen_generate_binary_compare(code, ast_binary_op, lhs_value, rhs_value);
    } else if (ast_binary_op->type == OR  ||
               ast_binary_op->type == AND ||
              (ast_binary_op->type >= PLUS && ast_binary_op->type <= MOD_BINARY_OP)) {
        return codegen_generate_binary_arithetic(code, ast_binary_op, lhs_value, rhs_value);
    }

    return NULL;
}

LLVMValueRef codegen_generate_if_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == IF_STMT);

    if_stmt* ast_if = &ast_node->as.iff;

    assert(ast_if);

    stmt cond_stmt = {0};
    cond_stmt.type = EXPRESSION_STMT;
    memcpy(&cond_stmt.as.exprr, &ast_if->condition, sizeof(expr));

    LLVMValueRef cond_value = codegen_generate_expr_stmt(code, &cond_stmt);
    if (!cond_value) {
        return NULL;
    }

    LLVMValueRef parent = LLVMGetBasicBlockParent(get_current_block(code));
    LLVMBasicBlockRef then_bb = LLVMAppendBasicBlockInContext(code->context, parent, "then");
    LLVMBasicBlockRef else_bb = LLVMCreateBasicBlockInContext(code->context, "else");
    LLVMBasicBlockRef cont_bb = LLVMCreateBasicBlockInContext(code->context, "cont");

    LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));
    LLVMBuildCondBr(code->ir_builder, cond_value, then_bb, else_bb);

    bool is_merge_needed  = false;

    push_existing_block(code, then_bb);
    {
        stmt then_body_stmt = {0};
        then_body_stmt.type = BLOCK_STMT;
        memcpy(&then_body_stmt.as.block, &ast_if->then_body, sizeof(block_stmt));

        LLVMValueRef then_value = codegen_generate_block_stmt(code, &then_body_stmt);
        if (!then_value) {
            return codegen_report_error(code, &ast_if->loc, "missing then body");
        }

        LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));
        LLVMValueRef then_terminator = LLVMGetBasicBlockTerminator(get_current_block(code));
        if (!then_terminator) {
            LLVMBuildBr(code->ir_builder, cont_bb);
            is_merge_needed = true;
        }

        LLVMAppendExistingBasicBlock(parent, else_bb);
    }
    pop_block(code);

    push_existing_block(code, else_bb);
    {
        LLVMValueRef else_value = NULL;
        if (ast_if->has_else) {
            stmt else_body_stmt = {0};
            else_body_stmt.type = BLOCK_STMT;
            memcpy(&else_body_stmt.as.block, &ast_if->else_body, sizeof(block_stmt));

            else_value = codegen_generate_block_stmt(code, &else_body_stmt);
        }

        LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));
        LLVMValueRef else_terminator = LLVMGetBasicBlockTerminator(get_current_block(code));
        if (!else_terminator) {
            LLVMBuildBr(code->ir_builder, cont_bb);
            is_merge_needed = true;
        }
    }
    pop_block(code);

    if (is_merge_needed) {
        LLVMAppendExistingBasicBlock(parent, cont_bb);
        LLVMPositionBuilderAtEnd(code->ir_builder, cont_bb);

        push_existing_block(code, cont_bb);
    }
    
    return LLVMBasicBlockAsValue(cont_bb);
}

LLVMValueRef codegen_generate_while_stmt(codegen *code, stmt *ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == WHILE_STMT);

    while_stmt* ast_while = &ast_node->as.whilee;

    assert(ast_while);

    LLVMValueRef parent = LLVMGetBasicBlockParent(get_current_block(code));
    LLVMBasicBlockRef condition_bb  = LLVMAppendBasicBlockInContext(code->context, parent, "cond");
    LLVMBasicBlockRef loop_bb       = LLVMCreateBasicBlockInContext(code->context, "loop");
    LLVMBasicBlockRef continue_bb   = LLVMCreateBasicBlockInContext(code->context, "cont");

    LLVMBuildBr(code->ir_builder, condition_bb);
    LLVMPositionBuilderAtEnd(code->ir_builder, condition_bb);

    push_existing_block(code, condition_bb);
    {
        stmt cond_stmt = {0};
        cond_stmt.type = EXPRESSION_STMT;
        memcpy(&cond_stmt.as.exprr, &ast_while->condition, sizeof(expr));

        LLVMValueRef cond_value = codegen_generate_expr_stmt(code, &cond_stmt);
        if (!cond_value) {
            return codegen_report_error(code, &ast_while->loc, "missing while loop condition");
        }

        LLVMBuildCondBr(code->ir_builder, cond_value, loop_bb, continue_bb);
    }
    pop_block(code);

    LLVMAppendExistingBasicBlock(parent, loop_bb);
    LLVMPositionBuilderAtEnd(code->ir_builder, loop_bb);

    push_existing_block(code, loop_bb);
    {
        stmt loop_body_stmt = {0};
        loop_body_stmt.type = BLOCK_STMT;
        memcpy(&loop_body_stmt.as.block, &ast_while->body, sizeof(block_stmt));

        LLVMValueRef loop_body_value = codegen_generate_block_stmt(code, &loop_body_stmt);
        if (!loop_body_value) {
            return codegen_report_error(code, &ast_while->loc, "missing while loop body");
        }

        LLVMBuildBr(code->ir_builder, condition_bb);
    }
    pop_block(code);

    {
        LLVMAppendExistingBasicBlock(parent, continue_bb);
        LLVMPositionBuilderAtEnd(code->ir_builder, continue_bb);

        push_existing_block(code, continue_bb);
    }

    return LLVMBasicBlockAsValue(continue_bb);
}

LLVMValueRef codegen_generate_do_stmt(codegen *code, stmt *ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == DO_STMT);

    (void)code;
    (void)ast_node;

    return NULL;
}

LLVMValueRef codegen_generate_break_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == BREAK_STMT);

    //use br label <dest> IR
    //in this case: br label [while/for exit]
    //LLVMValueRef LLVMBuildBr (LLVMBuilderRef, LLVMBasicBlockRef Dest)

    //return LLVMBuildBr(code->ir_builder, brk_dest);
    (void)code;
    (void)ast_node;

    return NULL;
}

LLVMValueRef codegen_generate_continue_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == CONTINUE_STMT);

    //use br label <dest> IR
    //in this case: br label [while/for condition]
    //LLVMValueRef LLVMBuildBr (LLVMBuilderRef, LLVMBasicBlockRef Dest)

    //return LLVMBuildBr(code->ir_builder, cont_dest);
    (void)code;
    (void)ast_node;

    return NULL;
}

LLVMValueRef codegen_generate_return_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == RETURN_STMT);

    return_stmt* ast_return = &ast_node->as.returnn;

    assert(ast_return);

    if (ast_return->has_result) {
        stmt ast_expr;
        ast_expr.type = EXPRESSION_STMT;
        memcpy(&ast_expr.as.exprr, &ast_return->result, sizeof(expr));

        LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));

        LLVMValueRef return_val = codegen_generate_expr_stmt(code, &ast_expr);
        if (!return_val)
            return NULL;

        return LLVMBuildRet(code->ir_builder, return_val);
    } else {
        return LLVMBuildRetVoid(code->ir_builder);
    }
}

LLVMValueRef codegen_generate_block_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == BLOCK_STMT);

    list_it      stmts_it;
    LLVMValueRef last_stmt;
    block_stmt*  ast_block = &ast_node->as.block;

    assert(ast_block);

    enter_scope();
    {
        for (stmts_it = ast_block->stmts.front;
             stmts_it != NULL;
             stmts_it = stmts_it->next) {
            // Dispatch
            stmt* ast_stmt = (stmt*) stmts_it->data;

            assert(ast_stmt->type >= IF_STMT &&
                   ast_stmt->type <= EXPRESSION_STMT);

            last_stmt = codegen_stmt_functions_table[ast_stmt->type](code, ast_stmt);
        }
    }
    exit_scope();

    return last_stmt;
}

static uint32_t get_type_size_in_bits(LLVMTypeKind kind, LLVMTypeRef type) {
    switch(kind) {
        case LLVMFloatTypeKind:   return 32;
        case LLVMDoubleTypeKind:  return 64;
        case LLVMIntegerTypeKind: return LLVMGetIntTypeWidth(type);

        default: {
            return 0;
        } break;
    }
}

LLVMValueRef codegen_generate_var_assign_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == VAR_ASSIGN_STMT);

    var_assign* ast_assign = &ast_node->as.var_assi;

    assert(ast_assign);

    stmt ast_expr;
    ast_expr.type = EXPRESSION_STMT;
    memcpy(&ast_expr.as.exprr, &ast_assign->value, sizeof(expr));

    LLVMValueRef rhs_value = codegen_generate_expr_stmt(code, &ast_expr);
    if (!rhs_value) {
        return NULL;
    }

    codegen_symbol* id;
    if ((id = symbol_table_get(&code->symbols, ast_assign->name.data)) == NULL) {
        return codegen_report_error(code,
                                    &ast_assign->loc,
                                    "undeclared variable '%s'",
                                    ast_assign->name.data);
    }

    LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));

    LLVMTypeRef rhs_type   = LLVMTypeOf(rhs_value);
    LLVMValueRef var_value = id->value;
    LLVMTypeRef  var_type  = id->type;

    LLVMTypeKind rhs_kind = LLVMGetTypeKind(rhs_type);
    LLVMTypeKind var_kind = LLVMGetTypeKind(var_type);
    if (rhs_kind != var_kind) {
        return codegen_report_error(code, &ast_assign->loc,
                                    "assignment of incompatible types");
    } else {
        uint32_t rhs_size = get_type_size_in_bits(rhs_kind, rhs_type);
        uint32_t var_size = get_type_size_in_bits(var_kind, var_type);

        if (rhs_size < var_size) {
            rhs_value = LLVMBuildZExtOrBitCast(code->ir_builder, rhs_value, var_type, "cast");
        } else if (rhs_size > var_size) {
            rhs_value = LLVMBuildTruncOrBitCast(code->ir_builder, rhs_value, var_type, "cast");
        }
    }

    return LLVMBuildStore(code->ir_builder, rhs_value, var_value);
}

static void print_symbol_table(const char* key, void* value, void* extra) {
    codegen_symbol* symbol = (codegen_symbol*) value;
    LLVMValueRef sym_value = symbol->value;
    LLVMTypeRef  sym_type  = symbol->type;

    fprintf(stdout, "\t[%s %p %p %d]\n", key, sym_value, sym_type, symbol->scope);
}

static bool check_for_var_in_scope(void* symbol, void* extra) {
    codegen_symbol* var = (codegen_symbol*) symbol;
    int32_t scope       = UNWRAP(int32_t, extra);

    if (var->scope == scope) {
        return true;
    }

    return false;
}

LLVMValueRef codegen_generate_var_definition_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == VAR_DEFINITION_STMT);

    var_def* ast_var_def = &ast_node->as.var_defi;

    assert(ast_var_def);


    // check for duplicate paramaters
    const char* var_name = ast_var_def->name.data;
    location*   var_loc  = &ast_var_def->loc;

    codegen_symbol_scope current_scope = get_current_scope();
    if (symbol_table_contains(&code->symbols, var_name, check_for_var_in_scope, (void*) &current_scope) == SYM_SUCCESS) {
        return codegen_report_error(code,
                                    var_loc,
                                    "trying to redifine variable '%s' in current scope.",
                                    var_name);
    }

    if (is_build_in_function(code, var_name)) {
        return codegen_report_error(code,
                                    var_loc,
                                    "trying to redifine build in function '%s' as variable.",
                                    var_name);
    }

    LLVMPositionBuilderAtEnd(code->ir_builder, get_current_block(code));

    // Get the var type and generate an AllocaInst.
    LLVMTypeRef  var_type  = code->types_to_llvm[ast_var_def->type];
    LLVMValueRef var_value = LLVMBuildAlloca(code->ir_builder, var_type, ast_var_def->name.data);

    codegen_symbol* var_symbol = (codegen_symbol*) arena_allocate(code->mem, sizeof(codegen_symbol));
    var_symbol->scope       = current_scope;
    var_symbol->is_function = false;

    var_symbol->value = arena_allocate(code->mem, sizeof(LLVMValueRef));
    memcpy(&var_symbol->value, &var_value, sizeof(LLVMValueRef));

    var_symbol->type  = arena_allocate(code->mem, sizeof(LLVMTypeRef));
    memcpy(&var_symbol->type, &var_type, sizeof(LLVMTypeRef));

    if (symbol_table_put(code->mem, &code->symbols, var_name, (void*) var_symbol) != SYM_SUCCESS) {
        return codegen_report_error(code,
                                    var_loc,
                                    "unable to create symbol '%s'.",
                                    var_name);
    }

#if 0
    fprintf(stdout, "Symbol Table: \n");
    symbol_table_map(&code->symbols, print_symbol_table, NULL);
    fprintf(stdout, "\n");
#endif

    // Assign
    if (ast_var_def->is_initialized) {
        stmt ast_assign_stmt;
        ast_assign_stmt.type = VAR_ASSIGN_STMT;

        var_assign ast_assign;
        ast_assign.name  = ast_var_def->name;
        ast_assign.loc   = ast_var_def->loc;
        ast_assign.value = ast_var_def->rhs;

        // Wrap it as stmt
        ast_assign_stmt.as.var_assi = ast_assign;

        codegen_generate_var_assign_stmt(code, &ast_assign_stmt);
    }

    return var_value;
}

LLVMValueRef codegen_generate_expr_stmt(codegen* code, stmt* ast_node) {
    assert(code);
    assert(ast_node && ast_node->type == EXPRESSION_STMT);

    expr* ast_expr = &ast_node->as.exprr;

    assert(ast_expr                       &&
           ast_expr->type >= LITERAL_EXPR &&
           ast_expr->type <= BINARY_OP_EXPR);

    return codegen_epxr_functions_table[ast_expr->type](code, ast_expr);
}

LLVMValueRef codegen_generate_func_decl(codegen* code, func_decl* ast_func) {
    assert(code);
    assert(ast_func);

    const char* func_name = ast_func->name.data;
    location*   func_loc  = &ast_func->loc;

    if (symbol_table_contains(&code->symbols, func_name, NULL, NULL) == SYM_SUCCESS) {
        const char* func_error_message = NULL;
        if (is_build_in_function(code, func_name)) {
            func_error_message = "trying to redeclare build in function '%s'.";
        } else {
            func_error_message = "trying to redeclare function '%s'.";
        }

        return codegen_report_error(code, func_loc, func_error_message, func_name);
    }

    LLVMTypeRef params_types[ast_func->parameters.length];

    list_it params_it;
    uint32_t param_num = 0;
    for (params_it = ast_func->parameters.front;
         params_it != NULL && param_num < ast_func->parameters.length;
         params_it = params_it->next, param_num++) {
        func_param* param = (func_param*) params_it->data;
        params_types[param_num] = code->types_to_llvm[param->type];
    }

    LLVMTypeRef function_type = LLVMFunctionType(code->types_to_llvm[ast_func->return_type],
                                                 params_types,
                                                 ast_func->parameters.length,
                                                 0);

    LLVMValueRef function_value = LLVMAddFunction(code->module,
                                                  func_name,
                                                  function_type);

    LLVMBasicBlockRef body = LLVMAppendBasicBlockInContext(code->context, function_value, "entry");

    codegen_symbol* func_symbol = (codegen_symbol*) arena_allocate(code->mem, sizeof(codegen_symbol));
    func_symbol->scope       = get_current_scope();
    func_symbol->is_function = true;

    func_symbol->value = arena_allocate(code->mem, sizeof(LLVMValueRef));
    memcpy(&func_symbol->value, &function_value, sizeof(LLVMValueRef));

    func_symbol->type = arena_allocate(code->mem, sizeof(LLVMTypeRef));
    memcpy(&func_symbol->type, &function_type, sizeof(LLVMTypeRef));

    if (symbol_table_put(code->mem, &code->symbols, func_name, (void*) func_symbol) != SYM_SUCCESS) {
        return codegen_report_error(code,
                                    func_loc,
                                    "unable to create symbol '%s'.",
                                    func_name);
    }

    push_existing_block(code, body);
    {
        enter_scope();
        {
            LLVMPositionBuilderAtEnd(code->ir_builder, body);

            LLVMValueRef param_it = LLVMGetFirstParam(function_value);
            list_it      params   = ast_func->parameters.front;

            while(params != NULL) {
                func_param* param = (func_param*) params->data;

                stmt ast_var_def_stmt = {0};
                ast_var_def_stmt.type = VAR_DEFINITION_STMT;

                var_def ast_var_def        = {0};
                ast_var_def.type           = param->type;
                ast_var_def.name           = param->name;
                ast_var_def.loc            = param->loc;
                ast_var_def.is_initialized = false;
                ast_var_def.is_const       = false;

                ast_var_def_stmt.as.var_defi = ast_var_def;

                LLVMValueRef param_value = codegen_generate_var_definition_stmt(code, &ast_var_def_stmt);
                if (param_value) {
                    LLVMSetValueName2(param_it, param->name.data, param->name.length);
                    LLVMBuildStore(code->ir_builder, param_value, param_it);
                }

                param_it = LLVMGetNextParam(param_it);
                params   = params->next;
            }
        }
        exit_scope();

        stmt ast_block;
        ast_block.type = BLOCK_STMT;
        memcpy(&ast_block.as.block, &ast_func->body, sizeof(block_stmt));

        codegen_generate_block_stmt(code, &ast_block);
    }
    pop_block(code);

    return function_value;
}

void codegen_generate_top_level(codegen* code, top_level* ast_top) {
    assert(code);
    assert(ast_top);
    assert(ast_top->type == VAR_DEF_TL ||
           ast_top->type == FUN_DECL_TL);

    switch (ast_top->type) {
        case VAR_DEF_TL: {
            stmt var_defi_stmt = {0};
            var_defi_stmt.type = VAR_DEFINITION_STMT;
            memcpy(&var_defi_stmt.as.var_defi, &ast_top->as.var, sizeof(var_def));

            codegen_generate_var_definition_stmt(code, &var_defi_stmt);
        } break;

        case FUN_DECL_TL: {
            codegen_generate_func_decl(code, &ast_top->as.func);
        } break;

        default: {
            assert(false && "should not be here.");
        } break;
    }
}

static void module_for_each(list_it it, void* extra) {
    top_level* top = (top_level*) it->data;
    codegen* code  = (codegen*) extra;

    codegen_generate_top_level(code, top);
}

void codegen_generate_module(codegen* code, module* ast_mod) {
    assert(code);
    assert(ast_mod);

    list_for_each(&ast_mod->modules, module_for_each, (void*) code);
}
