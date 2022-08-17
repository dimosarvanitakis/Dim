#include "llvm_generator.h"

//Dispatch table
static llvm_generator_expr_function* const llvm_generator_epxr_functions_table[EXPR_TYPES_COUNT] = {
    emit_llvm_literal_expr,
    emit_llvm_func_call_expr,
    emit_llvm_lvalue_expr,
    emit_llvm_binary_op_expr
};

static llvm_generator_stmt_function* const llvm_generator_stmt_functions_table[STMT_TYPES_COUNT] = {
    emit_llvm_if_stmt,
    emit_llvm_while_stmt,
    emit_llvm_do_stmt,
    emit_llvm_break_stmt,
    emit_llvm_continue_stmt,
    emit_llvm_return_stmt,
    emit_llvm_block_stmt,
    emit_llvm_var_assign_stmt,
    emit_llvm_var_definition_stmt,
    emit_llvm_expr_stmt,
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

static inline void enter_scope(llvm_generator* generator) {
    generator->scope++;
}

static inline void exit_scope(llvm_generator* generator) {
    generator->scope--;
}

static inline int32_t get_current_scope(llvm_generator* generator) {
    return generator->scope;
}

static inline void enter_loop_body(llvm_generator* generator) {
    generator->bc_allowed++;

    // create a new patch "level"
    list* new_scope_break = list_create(generator->arena);
    list_push_back(generator->arena, generator->break_list, new_scope_break);

    list* new_scope_continue = list_create(generator->arena);
    list_push_back(generator->arena, generator->continue_list, new_scope_continue);
}

static inline void exit_loop_body(llvm_generator* generator) {
    generator->bc_allowed--;

    // we assume that the patch in the current loop already happened.
    list_pop_back(generator->break_list);
    list_pop_back(generator->continue_list);
}

static inline bool is_break_continue_allowed(llvm_generator* generator) {
    return (generator->bc_allowed > 0);
}

static inline void push_bc_patch_info(llvm_generator* generator, list* to_be_patched, LLVMBasicBlockRef parent, LLVMValueRef instr) {
    bc_patch_info* patch = (bc_patch_info*) arena_allocate(generator->arena, sizeof(bc_patch_info));
    patch->instr  = instr;

    list_push_back(generator->arena, to_be_patched, patch);
}

static inline void push_new_block(llvm_generator* generator) {
    LLVMBasicBlockRef new_block        = LLVMCreateBasicBlockInContext(generator->context, "entry");
    LLVMBasicBlockRef* to_insert_block = (LLVMBasicBlockRef*) arena_allocate(generator->arena, sizeof(LLVMBasicBlockRef*));
    *to_insert_block = new_block;

    list_push_back(generator->arena, generator->blocks, to_insert_block);
}

static inline void push_existing_block(llvm_generator* generator, LLVMBasicBlockRef existing_block) {
    LLVMBasicBlockRef* to_insert_block = (LLVMBasicBlockRef*) arena_allocate(generator->arena, sizeof(LLVMBasicBlockRef*));
    *to_insert_block = existing_block;

    list_push_back(generator->arena, generator->blocks, to_insert_block);
}

static inline void pop_block(llvm_generator* generator) {
    list_pop_back(generator->blocks);
}

static inline LLVMBasicBlockRef get_current_block(llvm_generator* generator) {
    list_it current_block = list_back(generator->blocks);

    return UNWRAP(LLVMBasicBlockRef, current_block->data);
}

static bool is_build_in_function(llvm_generator* generator, const char* name) {
    for (int32_t func = 0; func < BUILD_IN_FUNCTIONS_COUNT; func++) {
        if (!strcmp(generator->build_in_functions[func].name, name)) {
            return true;
        }
    }
    return false;
}

static void append_build_in_function(llvm_generator* generator,
                                     size_t index,
                                     LLVMValueRef func_value,
                                     LLVMTypeRef  func_type,
                                     const char*  func_name,
                                     void*        func_address)
{
    generator->build_in_functions[index].function = func_value;
    generator->build_in_functions[index].name     = func_name;
    generator->build_in_functions[index].address  = func_address;

    llvm_symbol* func_symbol = (llvm_symbol*) arena_allocate(generator->arena, sizeof(llvm_symbol));
    func_symbol->scope = get_current_scope(generator);
    func_symbol->type  = func_type;
    func_symbol->value = func_value;

    if (symbol_table_put(generator->arena, generator->symbols, func_name, func_symbol) != SYM_SUCCESS) {
        fprintf(stderr, "Unable to create build in function '%s'.\n", func_name);
        exit(1);
    }
}

static void create_build_in_functions(llvm_generator* generator) {
    // print
    {
        LLVMTypeRef print_param_types[] = { generator->types_to_llvm[STR_VAR] };
        LLVMTypeRef print_type          = LLVMFunctionType(LLVMVoidTypeInContext(generator->context),
                                                       print_param_types, 1, 1);
        LLVMValueRef print_function     = LLVMAddFunction(generator->module, "print", print_type);

        append_build_in_function(generator, 0, print_function, print_type, "print", print);
    }

    // inputc
    {
        LLVMTypeRef inputc_param_types[] = {};
        LLVMTypeRef inputc_type          = LLVMFunctionType(LLVMInt8TypeInContext(generator->context),
                                                        inputc_param_types, 0, 0);
        LLVMValueRef inputc_function     = LLVMAddFunction(generator->module, "inputc", inputc_type);

        append_build_in_function(generator, 1, inputc_function, inputc_type, "inputc", inputc);
    }
}

llvm_generator llvm_generator_create(const char* module_name) {
    llvm_generator generator = {0};

    generator.context    = LLVMContextCreate();
    generator.module     = LLVMModuleCreateWithNameInContext(module_name, generator.context);
    generator.ir_builder = LLVMCreateBuilderInContext(generator.context);

    generator.arena   = arena_create("llvm_generator_arena");
    generator.symbols = symbol_table_create(generator.arena);

    generator.blocks        = list_create(generator.arena);
    generator.break_list    = list_create(generator.arena);
    generator.continue_list = list_create(generator.arena);

    generator.errors     = 0;
    generator.scope      = 0;
    generator.bc_allowed = 0;

    LLVMTypeRef types_to_llvm[VAR_TYPES_COUNT] = {
        LLVMInt8TypeInContext(generator.context),
        LLVMInt16TypeInContext(generator.context),
        LLVMInt32TypeInContext(generator.context),
        LLVMInt64TypeInContext(generator.context),
        LLVMInt8TypeInContext(generator.context),
        LLVMInt16TypeInContext(generator.context),
        LLVMInt32TypeInContext(generator.context),
        LLVMInt64TypeInContext(generator.context),
        LLVMFloatTypeInContext(generator.context),
        LLVMDoubleTypeInContext(generator.context),
        LLVMInt32TypeInContext(generator.context),
        LLVMPointerType(LLVMInt8TypeInContext(generator.context), 0), // string
        LLVMPointerType(LLVMInt8TypeInContext(generator.context), 0), // ptr
        LLVMVoidTypeInContext(generator.context), // void
        LLVMVoidTypeInContext(generator.context)  // none
    };
    memcpy(generator.types_to_llvm, types_to_llvm, VAR_TYPES_COUNT * sizeof(LLVMTypeRef));

    create_build_in_functions(&generator);

    return generator;
}

void llvm_generator_clear(llvm_generator* generator) {
    LLVMDisposeModule(generator->module);
    LLVMDisposeBuilder(generator->ir_builder);
    LLVMContextDispose(generator->context);

    arena_inspect(generator->arena);
    arena_free(generator->arena);
}

LLVMValueRef llvm_generator_report_error(llvm_generator* generator, location* loc, const char* format, ...) {
    fprintf(stderr, "%s:%u:%u ~ generator generatin error: ", loc->file, loc->line, loc->column);

    va_list vargs;
    va_start(vargs, format);
    vfprintf(stderr, format, vargs);
    fprintf(stderr, "\n");
    va_end(vargs);

    generator->errors++;

    return NULL;
}

LLVMValueRef emit_llvm_literal_expr(llvm_generator* generator, expr* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == LITERAL_EXPR);

    literal_expr* ast_literal = &ast_node->as.literal;

    assert(ast_literal                      &&
           ast_literal->type >= INTEGER_LIT &&
           ast_literal->type <= STRING_LIT);

    switch(ast_literal->type) {
        case INTEGER_LIT: {
            LLVMTypeRef  type  = LLVMInt32TypeInContext(generator->context);
            LLVMValueRef value = LLVMConstInt(type, ast_literal->as.int_val, 1);

            return value;
        } break;

        case DOUBLE_LIT: {
             LLVMTypeRef  type  = LLVMDoubleTypeInContext(generator->context);
             LLVMValueRef value = LLVMConstReal(type, ast_literal->as.double_val);

             return value;
        } break;

        case BOOL_LIT: {
            LLVMTypeRef  type = LLVMInt1TypeInContext(generator->context);
            LLVMValueRef value;
            if (ast_literal->as.bool_val)
                value = LLVMConstInt(type, 1, 0);
            else
                value = LLVMConstInt(type, 0, 0);

            return value;
        } break;

        case STRING_LIT: {
            int32_t size = strlen(ast_literal->as.string_val);

            LLVMTypeRef g_str_type = LLVMArrayType(LLVMInt8TypeInContext(generator->context), size + 1);
            LLVMValueRef g_str_val = LLVMAddGlobal(generator->module, g_str_type, ".str");
            LLVMSetGlobalConstant(g_str_val, 1);
            LLVMSetAlignment(g_str_val, 1);
            LLVMSetLinkage(g_str_val, LLVMPrivateLinkage);
            LLVMSetUnnamedAddress(g_str_val, LLVMGlobalUnnamedAddr);

            LLVMValueRef c_str_val = LLVMConstStringInContext(generator->context, ast_literal->as.string_val, size, 0);
            LLVMSetInitializer(g_str_val, c_str_val);

            LLVMValueRef zero_index = LLVMConstInt(LLVMInt64TypeInContext(generator->context), 0, true);
            LLVMValueRef indexes[]  = {zero_index, zero_index};

            LLVMValueRef pointer = LLVMBuildInBoundsGEP2(generator->ir_builder, g_str_type, g_str_val, indexes, 2, "");

            return pointer;
        } break;
    }
}

LLVMValueRef emit_llvm_func_call_expr(llvm_generator* generator, expr* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == FUNC_CALL_EXPR);

    func_call_expr* ast_func_call = &ast_node->as.func_call;

    assert(ast_func_call);

    LLVMValueRef callee_func = LLVMGetNamedFunction(generator->module, ast_func_call->name.data);
    if (!callee_func) {
        return llvm_generator_report_error(generator,
                                    &ast_func_call->loc,
                                    "trying to call an unknown function with name `%s`.",
                                    ast_func_call->name.data);
    }

    uint32_t callee_parama_size = LLVMCountParams(callee_func);
    uint32_t caller_pass_args   = ast_func_call->arguments->length;
    if (callee_parama_size != caller_pass_args) {
        return llvm_generator_report_error(generator,
                                    &ast_func_call->loc,
                                    "trying to call function `%s` with less parameters (expected %u and got %u)",
                                    ast_func_call->name.data,
                                    callee_parama_size,
                                    caller_pass_args);
    }

    LLVMValueRef args[callee_parama_size];

    list_it args_it = ast_func_call->arguments->front;
    uint32_t arg_n  = 0;
    while (args_it != NULL && arg_n < callee_parama_size) {
        expr* arg_expr = (expr*) args_it->data;

        stmt arg_stmt = {0};
        arg_stmt.type = EXPRESSION_STMT;
        memcpy(&arg_stmt.as.exprr, arg_expr, sizeof(expr));

        LLVMValueRef arg_expr_value = emit_llvm_expr_stmt(generator, &arg_stmt);

        args[arg_n] = arg_expr_value;

        args_it = args_it->next;
        arg_n   = arg_n + 1;
    }

    return LLVMBuildCall2(generator->ir_builder,
                          LLVMGetReturnType(LLVMTypeOf(callee_func)),
                          callee_func,
                          args,
                          callee_parama_size,
                          "calltmp");
}

LLVMValueRef emit_llvm_lvalue_expr(llvm_generator* generator, expr* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == LVALUE_EXPR);

    lvalue_expr* ast_lvalue = &ast_node->as.lvalue;

    assert(ast_lvalue);

    llvm_symbol* id;
    if ((id = (llvm_symbol*) symbol_table_get(generator->symbols, ast_lvalue->name.data))) {
        LLVMTypeRef  type  = id->type;
        LLVMValueRef value = id->value;

        return LLVMBuildLoad2(generator->ir_builder,
                              type,
                              value,
                              ast_lvalue->name.data);
    } else {
        return llvm_generator_report_error(generator, &ast_lvalue->loc,
                                    "undeclared variable `%s`.",
                                    ast_lvalue->name.data);
    }
}

static LLVMValueRef emit_llvm_binary_compare(llvm_generator* generator, binary_op_expr* ast_binary_op, LLVMValueRef lhs, LLVMValueRef rhs) {
    LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
    LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

    if (lhs_kind != LLVMFloatTypeKind  &&
        lhs_kind != LLVMDoubleTypeKind &&
        lhs_kind != LLVMIntegerTypeKind) {
        return llvm_generator_report_error(generator,
                                    &ast_binary_op->loc,
                                    "left hand side of compare expression is not a number.");
    }

    if (rhs_kind != LLVMFloatTypeKind  &&
        rhs_kind != LLVMDoubleTypeKind &&
        rhs_kind != LLVMIntegerTypeKind) {
        return llvm_generator_report_error(generator,
                                    &ast_binary_op->loc,
                                    "right hand side of compare expression is not a number.");
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
            return llvm_generator_report_error(generator,
                                        &ast_binary_op->loc,
                                        "unknown comparison operator `%s`.\n",
                                        binary_op_to_str_table[ast_binary_op->type]);
        } break;
    }

    if (is_real) {
        return LLVMBuildFCmp(generator->ir_builder, real_binary_op, lhs, rhs, "comptmp");
    } else {
        return LLVMBuildICmp(generator->ir_builder, int_binary_op, lhs, rhs, "comptmp");
    }
}

static inline bool is_floating_point_type(LLVMTypeKind kind) {
    return (kind == LLVMDoubleTypeKind ||
            kind == LLVMFloatTypeKind  ||
            kind == LLVMBFloatTypeKind);
}

static inline bool is_integer_type(LLVMTypeKind kind) {
    return (kind == LLVMIntegerTypeKind);
}

static inline uint32_t get_type_size_in_bits(LLVMTypeKind kind, LLVMTypeRef type) {
    switch(kind) {
        case LLVMFloatTypeKind:   return 32;
        case LLVMDoubleTypeKind:  return 64;
        case LLVMIntegerTypeKind: return LLVMGetIntTypeWidth(type);

        default: {
            return 0;
        } break;
    }
}

static LLVMValueRef emit_llvm_binary_arithetic(llvm_generator* generator, binary_op_expr* ast_binary_op, LLVMValueRef lhs, LLVMValueRef rhs) {
    LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
    LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

    bool lhs_is_float = is_floating_point_type(lhs_kind);
    bool rhs_is_float = is_floating_point_type(rhs_kind);
    bool lhs_is_integ = is_integer_type(lhs_kind);
    bool rhs_is_integ = is_integer_type(rhs_kind);

    uint32_t lhs_bits = get_type_size_in_bits(lhs_kind, LLVMTypeOf(lhs));
    uint32_t rhs_bits = get_type_size_in_bits(rhs_kind, LLVMTypeOf(rhs));

    if ((lhs_is_float && rhs_is_integ) ||
        (lhs_is_integ && rhs_is_float))
    {
        LLVMTypeRef double_type = LLVMDoubleTypeInContext(generator->context);
        LLVMOpcode  cast_op     = LLVMBitCast;

        {
            if (lhs_is_integ) {
                cast_op = LLVMSIToFP;
            } else {
                if (get_type_size_in_bits(lhs_kind, LLVMTypeOf(lhs)) < 64) {
                    cast_op = LLVMFPExt;
                }
            }
            lhs = LLVMBuildCast(generator->ir_builder, cast_op, lhs, double_type, "lhs_cast");
        }

        {
            if (rhs_is_integ) {
                cast_op = LLVMSIToFP;
            } else {
                if (get_type_size_in_bits(rhs_kind, LLVMTypeOf(rhs)) < 64) {
                    cast_op = LLVMFPExt;
                }
            }
            rhs  = LLVMBuildCast(generator->ir_builder, cast_op, rhs, double_type, "rhs_cast");
        }

    } else if (lhs_is_integ && rhs_is_integ) {
        if (lhs_bits > rhs_bits) {
            rhs = LLVMBuildCast(generator->ir_builder, LLVMSExt, rhs, LLVMTypeOf(lhs), "rhs_cast");
        } else if (lhs_bits < rhs_bits) {
            rhs = LLVMBuildCast(generator->ir_builder, LLVMSExt, lhs, LLVMTypeOf(rhs), "lhs_cast");
        }
    } else if (lhs_is_float && rhs_is_float) {
        if (lhs_bits > rhs_bits) {
            rhs = LLVMBuildCast(generator->ir_builder, LLVMFPExt, rhs, LLVMTypeOf(lhs), "rhs_cast");
        } else if (lhs_bits < rhs_bits) {
            rhs = LLVMBuildCast(generator->ir_builder, LLVMFPExt, lhs, LLVMTypeOf(rhs), "lhs_cast");
        }
    }

    bool is_real = is_floating_point_type(LLVMGetTypeKind(LLVMTypeOf(rhs)));
    if ((ast_binary_op->type == OR_BINARY_OP ||
         ast_binary_op->type == AND_BINARY_OP) && is_real) {
        return llvm_generator_report_error(generator,
                                    &ast_binary_op->loc,
                                    "the binary operators '||' and '&&' does not support floating point arithetic.\n");
    }

    LLVMOpcode arith_opgenerator;
    switch(ast_binary_op->type) {
        case OR_BINARY_OP: {
            arith_opgenerator = LLVMOr;
        } break;
        case AND_BINARY_OP:{
            arith_opgenerator = LLVMAnd;
        } break;

        case PLUS_BINARY_OP: {
            arith_opgenerator = is_real ? LLVMFAdd : LLVMAdd;
        } break;

        case MINUS_BINARY_OP: {
            arith_opgenerator = is_real ? LLVMFSub : LLVMSub;
        } break;

        case MULTI_BINARY_OP: {
            arith_opgenerator = is_real ? LLVMFMul : LLVMMul;
        } break;

        case DIV_BINARY_OP: {
            arith_opgenerator = is_real ? LLVMFDiv : LLVMSDiv;
        } break;

        case MOD_BINARY_OP: {
            arith_opgenerator = is_real ? LLVMFRem : LLVMSRem;
        } break;

        // Handle the rest of the cases as error
        default: {
            return llvm_generator_report_error(generator,
                                        &ast_binary_op->loc,
                                        "unknown arithetic operator `%s`.\n",
                                        binary_op_to_str_table[ast_binary_op->type]);
        } break;
    }

    return LLVMBuildBinOp(generator->ir_builder, arith_opgenerator, lhs, rhs, "arithtemp");
}

LLVMValueRef emit_llvm_binary_op_expr(llvm_generator *generator, expr *ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == BINARY_OP_EXPR);

    binary_op_expr* ast_binary_op = ast_node->as.binary_op;

    assert(ast_binary_op);

    stmt lhs_stmt = {0};
    lhs_stmt.type = EXPRESSION_STMT;
    memcpy(&lhs_stmt.as.exprr, &ast_binary_op->lhs, sizeof(expr));
    LLVMValueRef rhs_value = emit_llvm_expr_stmt(generator, &lhs_stmt);

    stmt rhs_stmt = {0};
    rhs_stmt.type = EXPRESSION_STMT;
    memcpy(&rhs_stmt.as.exprr, &ast_binary_op->rhs, sizeof(expr));
    LLVMValueRef lhs_value = emit_llvm_expr_stmt(generator, &rhs_stmt);

    if (!lhs_value || !rhs_value)
        return NULL;

    if (ast_binary_op->type >= EQUALS_BINARY_OP &&
        ast_binary_op->type <= LESS_BINARY_OP) {
        return emit_llvm_binary_compare(generator, ast_binary_op, lhs_value, rhs_value);
    } else if (ast_binary_op->type == OR  ||
               ast_binary_op->type == AND ||
              (ast_binary_op->type >= PLUS && ast_binary_op->type <= MOD_BINARY_OP)) {
        return emit_llvm_binary_arithetic(generator, ast_binary_op, lhs_value, rhs_value);
    }

    return NULL;
}

LLVMValueRef emit_llvm_if_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == IF_STMT);

    if_stmt* ast_if = &ast_node->as.iff;

    assert(ast_if);

    stmt cond_stmt = {0};
    cond_stmt.type = EXPRESSION_STMT;
    memcpy(&cond_stmt.as.exprr, &ast_if->condition, sizeof(expr));

    LLVMValueRef cond_value = emit_llvm_expr_stmt(generator, &cond_stmt);
    if (!cond_value) {
        return NULL;
    }

    LLVMValueRef parent = LLVMGetBasicBlockParent(get_current_block(generator));
    LLVMBasicBlockRef then_bb = LLVMAppendBasicBlockInContext(generator->context, parent, "then");
    LLVMBasicBlockRef else_bb = LLVMCreateBasicBlockInContext(generator->context, "else");
    LLVMBasicBlockRef cont_bb = LLVMCreateBasicBlockInContext(generator->context, "cont");

    LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));
    LLVMBuildCondBr(generator->ir_builder, cond_value, then_bb, else_bb);

    bool is_merge_needed = false;

    push_existing_block(generator, then_bb);
    {
        stmt then_body_stmt = {0};
        then_body_stmt.type = BLOCK_STMT;
        memcpy(&then_body_stmt.as.block, &ast_if->then_body, sizeof(block_stmt));

        LLVMValueRef then_value = emit_llvm_block_stmt(generator, &then_body_stmt);
        if (!then_value) {
            return llvm_generator_report_error(generator, &ast_if->loc, "missing then body");
        }

        LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));
        LLVMValueRef then_terminator = LLVMGetBasicBlockTerminator(get_current_block(generator));
        if (!then_terminator) {
            LLVMBuildBr(generator->ir_builder, cont_bb);
            is_merge_needed = true;
        }

        LLVMAppendExistingBasicBlock(parent, else_bb);
    }
    pop_block(generator);

    push_existing_block(generator, else_bb);
    {
        LLVMValueRef else_value = NULL;
        if (ast_if->has_else) {
            stmt else_body_stmt = {0};
            else_body_stmt.type = BLOCK_STMT;
            memcpy(&else_body_stmt.as.block, &ast_if->else_body, sizeof(block_stmt));

            else_value = emit_llvm_block_stmt(generator, &else_body_stmt);
        }

        LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));
        LLVMValueRef else_terminator = LLVMGetBasicBlockTerminator(get_current_block(generator));
        if (!else_terminator) {
            LLVMBuildBr(generator->ir_builder, cont_bb);
            is_merge_needed = true;
        }
    }
    pop_block(generator);

    if (is_merge_needed) {
        LLVMAppendExistingBasicBlock(parent, cont_bb);
        LLVMPositionBuilderAtEnd(generator->ir_builder, cont_bb);

        push_existing_block(generator, cont_bb);
    }
    
    return LLVMBasicBlockAsValue(cont_bb);
}

static void patch_bc_instructions(llvm_generator* generator, list* to_be_patched, LLVMBasicBlockRef patch) {
   for (list_it bc = to_be_patched->front;
        bc != NULL;
        bc = bc->next)
   {
       bc_patch_info* patch_info = (bc_patch_info*) bc->data;

       LLVMBasicBlockRef current_block = get_current_block(generator);

       LLVMPositionBuilderBefore(generator->ir_builder, patch_info->instr);
       LLVMBuildBr(generator->ir_builder, patch);
       LLVMInstructionRemoveFromParent(patch_info->instr);

       LLVMPositionBuilderAtEnd(generator->ir_builder, current_block);
   }
}

LLVMValueRef emit_llvm_while_stmt(llvm_generator *generator, stmt *ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == WHILE_STMT);

    while_stmt* ast_while = &ast_node->as.whilee;

    assert(ast_while);

    LLVMValueRef parent = LLVMGetBasicBlockParent(get_current_block(generator));
    LLVMBasicBlockRef condition_bb  = LLVMAppendBasicBlockInContext(generator->context, parent, "cond");
    LLVMBasicBlockRef loop_bb       = LLVMCreateBasicBlockInContext(generator->context, "loop");
    LLVMBasicBlockRef continue_bb   = LLVMCreateBasicBlockInContext(generator->context, "cont");

    LLVMBuildBr(generator->ir_builder, condition_bb);
    LLVMPositionBuilderAtEnd(generator->ir_builder, condition_bb);

    push_existing_block(generator, condition_bb);
    {
        stmt cond_stmt = {0};
        cond_stmt.type = EXPRESSION_STMT;
        memcpy(&cond_stmt.as.exprr, &ast_while->condition, sizeof(expr));

        LLVMValueRef cond_value = emit_llvm_expr_stmt(generator, &cond_stmt);
        if (!cond_value) {
            return llvm_generator_report_error(generator, &ast_while->loc, "missing while loop condition");
        }

        LLVMBuildCondBr(generator->ir_builder, cond_value, loop_bb, continue_bb);
    }
    pop_block(generator);

    LLVMAppendExistingBasicBlock(parent, loop_bb);
    LLVMPositionBuilderAtEnd(generator->ir_builder, loop_bb);

    push_existing_block(generator, loop_bb);
    {
        enter_loop_body(generator);
        {
            stmt loop_body_stmt = {0};
            loop_body_stmt.type = BLOCK_STMT;
            memcpy(&loop_body_stmt.as.block, &ast_while->body, sizeof(block_stmt));

            LLVMValueRef loop_body_value = emit_llvm_block_stmt(generator, &loop_body_stmt);
            if (!loop_body_value) {
                return llvm_generator_report_error(generator,
                                            &ast_while->loc,
                                            "missing while loop body");
            }

            list* to_be_patched_break    = (list*) list_back(generator->break_list)->data;
            list* to_be_patched_continue = (list*) list_back(generator->continue_list)->data;

            // patch break and continue instructions
            patch_bc_instructions(generator, to_be_patched_break, continue_bb);
            patch_bc_instructions(generator, to_be_patched_continue, condition_bb);
        }
        exit_loop_body(generator);

        LLVMBuildBr(generator->ir_builder, condition_bb);
    }
    pop_block(generator);

    {
        LLVMAppendExistingBasicBlock(parent, continue_bb);
        LLVMPositionBuilderAtEnd(generator->ir_builder, continue_bb);

        push_existing_block(generator, continue_bb);
    }

    return LLVMBasicBlockAsValue(continue_bb);
}

LLVMValueRef emit_llvm_do_stmt(llvm_generator *generator, stmt *ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == DO_STMT);

    (void)generator;
    (void)ast_node;

    return NULL;
}

LLVMValueRef emit_llvm_break_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == BREAK_STMT);

    break_stmt* ast_break = &ast_node->as.brk;

    if (!is_break_continue_allowed(generator)) {
        return llvm_generator_report_error(generator,
                                    &ast_break->loc,
                                    "break statement is only allowed inside a loop body.");
    }

    LLVMValueRef break_value       = LLVMBuildBr(generator->ir_builder, get_current_block(generator));
    LLVMBasicBlockRef break_parent = get_current_block(generator);

    list* current_break_patch_list = (list*) list_back(generator->break_list)->data;
    push_bc_patch_info(generator, current_break_patch_list, break_parent, break_value);

    // for now generate a br instruction with a dummy label that we will
    // patch later.
    return break_value;
}

LLVMValueRef emit_llvm_continue_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == CONTINUE_STMT);

    continue_stmt* ast_cont = &ast_node->as.cont;

    if (!is_break_continue_allowed(generator)) {
        return llvm_generator_report_error(generator,
                                    &ast_cont->loc,
                                    "continue statement is only allowed inside a loop body.");
    }

    LLVMValueRef continue_value = LLVMBuildBr(generator->ir_builder, get_current_block(generator));
    LLVMBasicBlockRef continue_parent = get_current_block(generator);

    list* current_continue_patch_list = (list*) list_back(generator->continue_list)->data;
    push_bc_patch_info(generator, current_continue_patch_list, continue_parent, continue_value);

    // for now generate a br instruction with a dummy label that we will
    // patch later.
    return continue_value;
}

LLVMValueRef emit_llvm_return_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == RETURN_STMT);

    return_stmt* ast_return = &ast_node->as.returnn;

    assert(ast_return);

    if (ast_return->has_result) {
        stmt ast_expr;
        ast_expr.type = EXPRESSION_STMT;
        memcpy(&ast_expr.as.exprr, &ast_return->result, sizeof(expr));

        LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));

        LLVMValueRef return_val = emit_llvm_expr_stmt(generator, &ast_expr);
        if (!return_val)
            return NULL;

        return LLVMBuildRet(generator->ir_builder, return_val);
    } else {
        return LLVMBuildRetVoid(generator->ir_builder);
    }
}

LLVMValueRef emit_llvm_block_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == BLOCK_STMT);

    LLVMValueRef last_stmt;
    block_stmt*  ast_block = &ast_node->as.block;

    assert(ast_block);

    enter_scope(generator);
    {
        for (list_it it = ast_block->stmts->front;
             it != NULL;
             it = it->next)
        {
            // Dispatch
            stmt* ast_stmt = (stmt*) it->data;

            assert(ast_stmt->type >= IF_STMT &&
                   ast_stmt->type <= EXPRESSION_STMT);

            last_stmt = llvm_generator_stmt_functions_table[ast_stmt->type](generator, ast_stmt);
        }
    }
    exit_scope(generator);

    return last_stmt;
}

LLVMValueRef emit_llvm_var_assign_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == VAR_ASSIGN_STMT);

    var_assign* ast_assign = &ast_node->as.var_assi;

    assert(ast_assign);

    stmt ast_expr;
    ast_expr.type = EXPRESSION_STMT;
    memcpy(&ast_expr.as.exprr, &ast_assign->value, sizeof(expr));

    LLVMValueRef rhs_value = emit_llvm_expr_stmt(generator, &ast_expr);
    if (!rhs_value) {
        return NULL;
    }

    llvm_symbol* id;
    if ((id = symbol_table_get(generator->symbols, ast_assign->name.data)) == NULL) {
        return llvm_generator_report_error(generator,
                                    &ast_assign->loc,
                                    "undeclared variable '%s'",
                                    ast_assign->name.data);
    }

    LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));

    LLVMTypeRef rhs_type   = LLVMTypeOf(rhs_value);
    LLVMValueRef var_value = id->value;
    LLVMTypeRef  var_type  = id->type;

    LLVMTypeKind rhs_kind = LLVMGetTypeKind(rhs_type);
    LLVMTypeKind var_kind = LLVMGetTypeKind(var_type);
    if (rhs_kind != var_kind) {
        return llvm_generator_report_error(generator, &ast_assign->loc,
                                    "assignment of incompatible types");
    } else {
        uint32_t rhs_size = get_type_size_in_bits(rhs_kind, rhs_type);
        uint32_t var_size = get_type_size_in_bits(var_kind, var_type);

        if (rhs_size < var_size) {
            rhs_value = LLVMBuildSExtOrBitCast(generator->ir_builder, rhs_value, var_type, "cast");
        } else if (rhs_size > var_size) {
            rhs_value = LLVMBuildTruncOrBitCast(generator->ir_builder, rhs_value, var_type, "cast");
        }
    }

    return LLVMBuildStore(generator->ir_builder, rhs_value, var_value);
}

static void print_symbol_table(const char* key, void* value, void* extra) {
    llvm_symbol* symbol = (llvm_symbol*) value;
    LLVMValueRef sym_value = symbol->value;
    LLVMTypeRef  sym_type  = symbol->type;

    fprintf(stdout, "\t[%s %p %p %d]\n", key, sym_value, sym_type, symbol->scope);
}

static bool check_for_var_in_scope(void* symbol, void* extra) {
    llvm_symbol* var = (llvm_symbol*) symbol;
    int32_t scope    = UNWRAP(int32_t, extra);

    if (var->scope == scope) {
        return true;
    }

    return false;
}

static inline bool is_var_unsigned(var_type type) {
    return (type >= U8_VAR && type <= U64_VAR);
}

LLVMValueRef emit_llvm_var_definition_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == VAR_DEFINITION_STMT);

    var_def* ast_var_def = &ast_node->as.var_defi;

    assert(ast_var_def);

    // check for duplicate paramaters
    const char* var_name = ast_var_def->name.data;
    location*   var_loc  = &ast_var_def->loc;

    int32_t current_scope = get_current_scope(generator);
    if (symbol_table_contains(generator->symbols, var_name, check_for_var_in_scope, (void*) &current_scope) == SYM_SUCCESS) {
        return llvm_generator_report_error(generator,
                                    var_loc,
                                    "trying to redifine variable '%s' in current scope.",
                                    var_name);
    }

    if (is_build_in_function(generator, var_name)) {
        return llvm_generator_report_error(generator,
                                    var_loc,
                                    "trying to redifine build in function '%s' as variable.",
                                    var_name);
    }

    LLVMPositionBuilderAtEnd(generator->ir_builder, get_current_block(generator));

    // Get the var type and generate an AllocaInst.
    LLVMTypeRef  var_type  = generator->types_to_llvm[ast_var_def->type];
    LLVMValueRef var_value = LLVMBuildAlloca(generator->ir_builder, var_type, ast_var_def->name.data);

    llvm_symbol* var_symbol = (llvm_symbol*) arena_allocate(generator->arena, sizeof(llvm_symbol));
    var_symbol->scope       = current_scope;
    var_symbol->type        = var_type;
    var_symbol->value       = var_value;

    if (symbol_table_put(generator->arena, generator->symbols, var_name, (void*) var_symbol) != SYM_SUCCESS) {
        return llvm_generator_report_error(generator,
                                    var_loc,
                                    "unable to create symbol '%s'.",
                                    var_name);
    }

#if 0
    fprintf(stdout, "Symbol Table: \n");
    symbol_table_map(&generator->symbols, print_symbol_table, NULL);
    fprintf(stdout, "\n");
#endif

    if (ast_var_def->is_initialized) {
        stmt ast_assign_stmt;
        ast_assign_stmt.type = VAR_ASSIGN_STMT;

        var_assign ast_assign;
        ast_assign.name  = ast_var_def->name;
        ast_assign.loc   = ast_var_def->loc;
        ast_assign.value = ast_var_def->rhs;

        ast_assign_stmt.as.var_assi = ast_assign;

        emit_llvm_var_assign_stmt(generator, &ast_assign_stmt);
    }

    return var_value;
}

LLVMValueRef emit_llvm_expr_stmt(llvm_generator* generator, stmt* ast_node) {
    assert(generator);
    assert(ast_node && ast_node->type == EXPRESSION_STMT);

    expr* ast_expr = &ast_node->as.exprr;

    assert(ast_expr                       &&
           ast_expr->type >= LITERAL_EXPR &&
           ast_expr->type <= BINARY_OP_EXPR);

    return llvm_generator_epxr_functions_table[ast_expr->type](generator, ast_expr);
}

LLVMValueRef emit_llvm_func_decl(llvm_generator* generator, func_decl* ast_func) {
    assert(generator);
    assert(ast_func);

    const char* func_name = ast_func->name.data;
    location*   func_loc  = &ast_func->loc;

    if (symbol_table_contains(generator->symbols, func_name, NULL, NULL) == SYM_SUCCESS) {
        const char* func_error_message = NULL;
        if (is_build_in_function(generator, func_name)) {
            func_error_message = "trying to redeclare build in function '%s'.";
        } else {
            func_error_message = "trying to redeclare function '%s'.";
        }

        return llvm_generator_report_error(generator, func_loc, func_error_message, func_name);
    }

    LLVMTypeRef params_types[ast_func->parameters->length];

    list_it params_it;
    uint32_t param_num = 0;
    for (params_it = ast_func->parameters->front;
         params_it != NULL && param_num < ast_func->parameters->length;
         params_it = params_it->next, param_num++) {
        func_param* param = (func_param*) params_it->data;
        params_types[param_num] = generator->types_to_llvm[param->type];
    }

    LLVMTypeRef function_type = LLVMFunctionType(generator->types_to_llvm[ast_func->return_type],
                                                 params_types,
                                                 ast_func->parameters->length,
                                                 0);

    LLVMValueRef function_value = LLVMAddFunction(generator->module,
                                                  func_name,
                                                  function_type);

    LLVMBasicBlockRef body = LLVMAppendBasicBlockInContext(generator->context, function_value, "entry");

    llvm_symbol* func_symbol = (llvm_symbol*) arena_allocate(generator->arena, sizeof(llvm_symbol));
    func_symbol->scope = get_current_scope(generator);
    func_symbol->type  = function_type;
    func_symbol->value = function_value;

    if (symbol_table_put(generator->arena, generator->symbols, func_name, (void*) func_symbol) != SYM_SUCCESS) {
        return llvm_generator_report_error(generator,
                                    func_loc,
                                    "unable to create symbol '%s'.",
                                    func_name);
    }

    push_existing_block(generator, body);
    {
        enter_scope(generator);
        {
            LLVMPositionBuilderAtEnd(generator->ir_builder, body);

            LLVMValueRef param_it = LLVMGetFirstParam(function_value);
            list_it      params   = ast_func->parameters->front;

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

                LLVMValueRef param_value = emit_llvm_var_definition_stmt(generator, &ast_var_def_stmt);
                if (param_value) {
                    LLVMSetValueName2(param_it, param->name.data, param->name.length);
                    LLVMBuildStore(generator->ir_builder, param_it, param_value);
                }

                param_it = LLVMGetNextParam(param_it);
                params   = params->next;
            }
        }
        exit_scope(generator);

        stmt ast_block;
        ast_block.type = BLOCK_STMT;
        memcpy(&ast_block.as.block, &ast_func->body, sizeof(block_stmt));

        LLVMValueRef body_value = emit_llvm_block_stmt(generator, &ast_block);

        // if no return has been generated yet...
        if (!LLVMGetBasicBlockTerminator(get_current_block(generator))) {

            // AND the return type is void generate a return void instruction.
            if (ast_func->return_type == VOID_VAR) {
                LLVMBuildRetVoid(generator->ir_builder);
            // else create a return instruction depending on the return value of the body.
            } else if (LLVMGetTypeKind(LLVMTypeOf(body_value)) != LLVMVoidTypeKind) {
                LLVMBuildRet(generator->ir_builder, body_value);
            }
        }
    }
    pop_block(generator);

    return function_value;
}

void emit_llvm_top_level(llvm_generator* generator, top_level* ast_top) {
    assert(generator);
    assert(ast_top);
    assert(ast_top->type == VAR_DEF_TL ||
           ast_top->type == FUN_DECL_TL);

    switch (ast_top->type) {
        case VAR_DEF_TL: {
            stmt var_defi_stmt = {0};
            var_defi_stmt.type = VAR_DEFINITION_STMT;
            memcpy(&var_defi_stmt.as.var_defi, &ast_top->as.var, sizeof(var_def));

            emit_llvm_var_definition_stmt(generator, &var_defi_stmt);
        } break;

        case FUN_DECL_TL: {
            emit_llvm_func_decl(generator, &ast_top->as.func);
        } break;

        default: {
            assert(false && "should not be here.");
        } break;
    }
}

static void module_for_each(list_it it, void* extra) {
    top_level* top = (top_level*) it->data;
    llvm_generator* generator  = (llvm_generator*) extra;

    emit_llvm_top_level(generator, top);
}

void emit_llvm_module(llvm_generator* generator, module* ast_mod) {
    assert(generator);
    assert(ast_mod);

    // We need to create a global function for the builder to start
    // generating ir in case we have global variables.
    LLVMTypeRef main_params[]    = {};
    LLVMTypeRef main_func_type   = LLVMFunctionType(LLVMVoidTypeInContext(generator->context), main_params, 0, false);
    LLVMValueRef main_func       = LLVMAddFunction(generator->module, "global", main_func_type);
    LLVMBasicBlockRef main_block = LLVMAppendBasicBlockInContext(generator->context, main_func, "entry");

    push_existing_block(generator, main_block);
    {
        list_for_each(ast_mod->modules, module_for_each, (void*) generator);
    }
    pop_block(generator);
}
