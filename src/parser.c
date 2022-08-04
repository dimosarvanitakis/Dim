#include "parser.h"

#include <stdio.h>

#define MAX_BIN_OP_PRECEDENCE 60

// Precedence 0 --> ||
// Precedence 10 --> &&
// Precedence 20 --> != ==
// Precedence 30 --> >= <= > <
// Precedence 40 --> + -
// Precedence 50 --> / * %
typedef struct binary_op_precedence {
	token_type op;
	int32_t    precedence;
} binary_op_precedence;

static binary_op_precedence op_precedence[13] = {
    {OR            , 0},
	{AND           , 10},
	{EQUALS        , 20},
	{NOT_EQUALS    , 20},
	{GREATER       , 30},
	{GREATER_EQUAL , 30},
	{LESS          , 30},
	{LESS_EQUAL    , 30},
	{PLUS          , 40},
	{MINUS         , 40},
	{MULTI         , 50},
	{DIV           , 50},
	{MOD           , 50}
};

static int32_t get_binary_op_precedence(token* token) {
	for (int32_t it = 0; it < 13; it++) {
		if (op_precedence[it].op == token->type) {
			return op_precedence[it].precedence;
		}
	}
	// Max precedence (primary, unary, oparators)
	return MAX_BIN_OP_PRECEDENCE;
}

static var_type convert_string_to_var_type(string* v_type) {
	const char* type = (const char*) v_type->data;

	if (!strcmp(type, "u8")) {
		return U8_VAR;
	} else if (!strcmp(type, "u16")) {
		return U16_VAR;
	} else if (!strcmp(type, "u32")) {
		return U32_VAR;
	} else if (!strcmp(type, "u64")) {
		return U64_VAR;
	} else if (!strcmp(type, "i8")) {
		return I8_VAR;
	} else if (!strcmp(type, "i16")) {
		return I16_VAR;
	} else if (!strcmp(type, "i32")) {
		return I32_VAR;
	} else if (!strcmp(type, "i64")) {
		return I64_VAR;
	} else if (!strcmp(type, "f32")) {
		return F32_VAR;
	} else if (!strcmp(type, "f64")) {
		return F64_VAR;
	} else if (!strcmp(type, "bool")) {
		return BOOL_VAR;
	} else if (!strcmp(type, "str")) {
		return STR_VAR;
    } else if (!strcmp(type, "ptr")){
		return PTR_VAR;
	} else if (strcmp(type, "void")) {
		return VOID_VAR;
	} else {
		return NONE_TYPE;
	}
}

// func_call_expr -> |
//                   | 'ID' '(' epxr [,expr]* ')'
static func_call_expr parse_func_call_expr(arena *mem, lexer *lex) {
	func_call_expr func_call = {0};

	token* curr_token   = lexer_get_current_token(lex);
	func_call.loc       = curr_token->loc;
	func_call.name      = curr_token->value;
	func_call.arguments = list_create(sizeof(expr));

	lexer_expect_token(lex, ID);
	lexer_expect_token(lex, OPEN_PARENTHESIS);

	curr_token = lexer_get_current_token(lex);
	if (curr_token->type != CLOSE_PARENTHESIS) {

		expr argument = {0};
		argument = parse_expr(mem, lex);

		list_push_back(mem, &func_call.arguments, (void*) &argument);

		curr_token = lexer_get_current_token(lex);
		while (curr_token->type == COMMA) {
			lexer_expect_token(lex, COMMA);

            argument = parse_expr(mem, lex);
			list_push_back(mem, &func_call.arguments, (void*) &argument);

			curr_token = lexer_get_current_token(lex);
		}
	}

	lexer_expect_token(lex, CLOSE_PARENTHESIS);

	return func_call;
}

// lvalue_expr -> |
//                | ID
static lvalue_expr parse_lvalue_expr(lexer* lex) {
	token* curr_token = lexer_get_current_token(lex);

	lexer_expect_token(lex, ID);

	lvalue_expr lvalue = {0};
	lvalue.loc  = curr_token->loc;
	lvalue.name = curr_token->value;

	return lvalue;
}

// (TODO): unary op
// primary_expr -> |
//                 | integer
//                 | real
//                 | string
//                 | true
//                 | false
//                 | nil
//                 | func_call_expr
//                 | lvalue_expr
//                 | '(' expr ')'
static expr parse_primary_expr(arena* mem, lexer* lex) {
 	token* curr_token = lexer_get_current_token(lex);

	switch(curr_token->type) {
		case EOFF: {
			lexer_report_error(&curr_token->loc,
							   "expected expression but got end of file.\n");
		} break;

		// literals
		case INTEGER: {
			lexer_eat_token(lex);

			char* temp;
			expr int_lit = {0};
			int_lit.loc                    = curr_token->loc;
			int_lit.type				   = LITERAL_EXPR;
			int_lit.as.literal.type		   = INTEGER_LIT;
			int_lit.as.literal.as.int_val  = strtol(curr_token->value.data, &temp, 10);

			return int_lit;
		} break;

		case REAL: {

			lexer_eat_token(lex);

			char* temp;
			expr real_lit = {0};
			real_lit.loc                     = curr_token->loc;
			real_lit.type					 = LITERAL_EXPR;
			real_lit.as.literal.type		 = DOUBLE_LIT;
			real_lit.as.literal.as.int_val   = strtod(curr_token->value.data, &temp);

			return real_lit;
		} break;

		case TRUE: {
			lexer_eat_token(lex);

			expr true_lit = {0};
			true_lit.loc                      = curr_token->loc;
			true_lit.type					  = LITERAL_EXPR;
			true_lit.as.literal.type		  = BOOL_LIT;
			true_lit.as.literal.as.bool_val   = true;

			return true_lit;
		} break;

		case FALSE: {
			lexer_eat_token(lex);

			expr false_lit = {0};
			false_lit.loc                      = curr_token->loc;
			false_lit.type					   = LITERAL_EXPR;
			false_lit.as.literal.type		   = BOOL_LIT;
			false_lit.as.literal.as.bool_val   = false;

			return false_lit;
		} break;

		case STRING: {
			lexer_eat_token(lex);

			expr string_lit = {0};
			string_lit.loc                       = curr_token->loc;
			string_lit.type					     = LITERAL_EXPR;
			string_lit.as.literal.type		     = STRING_LIT;
			string_lit.as.literal.as.string_val  = curr_token->value.data;

			return string_lit;
		} break;

		// func_call OR lvalue
		case ID: {

			// Function call
			expr id = {0};
			id.loc  = curr_token->loc;

			token* ahead = lexer_look_ahead(lex, 1);
			if (ahead->type == OPEN_PARENTHESIS) {
				id.type         = FUNC_CALL_EXPR;
				id.as.func_call = parse_func_call_expr(mem, lex);
			// lvalue
			} else {
				id.type      = LVALUE_EXPR;
				id.as.lvalue = parse_lvalue_expr(lex);
			}

			return id;
		} break;

		// '(' expr ')'
		case OPEN_PARENTHESIS: {
			lexer_eat_token(lex);

			expr par_expr = {0};
			par_expr.loc  = curr_token->loc;
			par_expr      = parse_expr(mem, lex);

			lexer_expect_token(lex, CLOSE_PARENTHESIS);

			return par_expr;
		} break;

		// Something went wrong
		default: {
			assert(false);
		} break;
	}
}

// Use TDOP (Top Down Operator Precedence)
// (TODO): https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html
// https://en.wikipedia.org/wiki/Operator-precedence_parser
// http://crockford.com/javascript/tdop/tdop.html
// https://kevinushey.github.io/blog/2016/02/12/top-down-operator-precedence-parsing-with-r/
// https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
static expr parse_expr_with_precedence(arena* mem, lexer* lex, int32_t precedence) {
	if (precedence >= MAX_BIN_OP_PRECEDENCE) {
		return parse_primary_expr(mem, lex);
	}

	expr lhs = parse_expr_with_precedence(mem, lex, precedence + 10);

	token* curr_token = lexer_get_current_token(lex);
	while ( (is_token_binary_operator(curr_token)) &&
		    (get_binary_op_precedence(curr_token) == precedence)) {

		lexer_eat_token(lex);

		expr temp_expr = {0};
		binary_op_expr* binary_op = arena_allocate(mem, sizeof(binary_op_expr));

		temp_expr.loc  = curr_token->loc;
		temp_expr.type = BINARY_OP_EXPR;

		binary_op->type = (binary_op_type) curr_token->type;
		binary_op->loc  = curr_token->loc;
		binary_op->lhs  = lhs;
		binary_op->rhs  = parse_expr_with_precedence(mem, lex, precedence + 10);

		temp_expr.as.binary_op = binary_op;

		lhs = temp_expr;

		curr_token = lexer_get_current_token(lex);
	}

	return lhs;
}

expr parse_expr(arena* mem, lexer* lex) {
	return parse_expr_with_precedence(mem, lex, 0);
}

// if_stmt ->    |
//               | 'if' '(' expr ')' then_body else_body
//
// then_body ->  |
//               | block_stmt

// else_body ->  |
//               | 'else' block_stmt
if_stmt parse_if_stmt(arena* mem, lexer* lex) {
	if_stmt iff = {0};

	token* curr_token = lexer_expect_token(lex, IF);
	lexer_expect_token(lex, OPEN_PARENTHESIS);

	iff.loc       = curr_token->loc;
	iff.condition = parse_expr(mem, lex);

	lexer_expect_token(lex, CLOSE_PARENTHESIS);

	iff.then_body = parse_block_stmt(mem, lex);

	// check for else
	curr_token = lexer_get_current_token(lex);
	if (curr_token->type == ELSE) {
		lexer_eat_token(lex);

		iff.has_else  = true;
		iff.else_body = parse_block_stmt(mem, lex);
	}

	return iff;
}

// while_stmt -> |
//               | 'while' '(' expr ')' block_stmt
//               | 'while' '(' expr ')' ';'
while_stmt parse_while_stmt(arena* mem, lexer* lex) {
	while_stmt whilee = {0};

	token* curr_token = lexer_expect_token(lex, WHILE);
	lexer_expect_token(lex, OPEN_PARENTHESIS);

	whilee.loc       = curr_token->loc;
	whilee.condition = parse_expr(mem, lex);

	lexer_expect_token(lex, CLOSE_PARENTHESIS);

	// no body
	curr_token = lexer_get_current_token(lex);
	if (curr_token->type == SEMICOLON) {
		lexer_eat_token(lex);

		return whilee;
	}

	whilee.body = parse_block_stmt(mem, lex);

	return whilee;
}

// do_stmt -> |
//            | 'do' block_stmt 'while' '(' expr ')'
do_stmt parse_do_stmt(arena* mem, lexer* lex) {
	do_stmt doo = {0};

	token* curr_token = lexer_expect_token(lex, DO);

	doo.body = parse_block_stmt(mem, lex);
	doo.loc  = curr_token->loc;

	lexer_expect_token(lex, WHILE);
	lexer_expect_token(lex, OPEN_PARENTHESIS);

	doo.condition = parse_expr(mem, lex);

	lexer_expect_token(lex, CLOSE_PARENTHESIS);

	return doo;
}

// break_stmt -> |
//               | 'break' ';'
break_stmt parse_break_stmt(lexer* lex) {
	break_stmt brk = {0};

 	token* curr_token = lexer_expect_token(lex, BREAK);
	brk.loc = curr_token->loc;

	lexer_expect_token(lex, SEMICOLON);

	return brk;
}

// continue_stmt -> |
//                  | 'continue' ';'
continue_stmt parse_continue_stmt(lexer* lex) {
	continue_stmt cont = {0};

 	token* curr_token = lexer_expect_token(lex, CONTINUE);
	cont.loc = curr_token->loc;

	lexer_expect_token(lex, SEMICOLON);

	return cont;
}

// return_stmt -> |
//                | 'return' expr ';'
return_stmt parse_return_stmt(arena* mem, lexer* lex) {
	return_stmt ret = {0};

	token* curr_token = lexer_expect_token(lex, RETURN);
	ret.loc = curr_token->loc;

	curr_token = lexer_get_current_token(lex);
	if (curr_token->type == SEMICOLON) {
		lexer_eat_token(lex);

		ret.has_result = false;
		return ret;
	}

	ret.has_result = true;
	ret.result     = parse_expr(mem, lex);

	lexer_expect_token(lex, SEMICOLON);

	return ret;
}

// block_stmt -> |
//               | '{' stmts* '}'
block_stmt parse_block_stmt(arena* mem, lexer* lex) {
	block_stmt block = {0};
	block.stmts = list_create(sizeof(stmt));

	lexer_expect_token(lex, OPEN_BRACE);

	token* curr_token = lexer_get_current_token(lex);
	while (curr_token->type != CLOSE_BRACE) {
		stmt stm;
		stm = parse_stmt(mem, lex);

		list_push_back(mem, &block.stmts, (void*) &stm);

		curr_token = lexer_get_current_token(lex);
	}

	lexer_expect_token(lex, CLOSE_BRACE);

	return block;
}

// var_assign -> |
//               | ID '=' expr ';'
var_assign parse_var_assign(arena* mem, lexer* lex) {
	var_assign var = {0};

	token* curr_token = lexer_expect_token(lex, ID);
	var.loc = curr_token->loc;
	var.name= curr_token->value;

	lexer_expect_token(lex, ASSIGN);

	var.value = parse_expr(mem, lex);

	lexer_expect_token(lex, SEMICOLON);

	return var;
}

//(TODO) : for statement
// 1) for_stmt -> |
//                | for ID in NUMBER..NUMBER block_stmt
//
// 2) for_stmt -> |
//                |

// stmt -> |
//         | expr_stmt
//         | if_stmt
//         | whiles_tmt
//         | do_stmt
//         | break_stmt
//         | continue_stmt
//         | return_stmt
//         | block_stmt
// 		   | var_decl
// 		   | var_assign
stmt parse_stmt(arena* mem, lexer* lex) {
	token* curr_token = lexer_get_current_token(lex);
	switch(curr_token->type) {
		case IF: {
			stmt result = {0};

			result.type   = IF_STMT;
			result.as.iff = parse_if_stmt(mem, lex);

			return result;
		} break;

		case WHILE: {
			stmt result = {0};

			result.type      = WHILE_STMT;
			result.as.whilee = parse_while_stmt(mem, lex);

			return result;
		} break;

		case DO: {
			stmt result = {0};

			result.type    = DO_STMT;
			result.as.doo = parse_do_stmt(mem, lex);

			return result;
		} break;

		case BREAK: {
			stmt result = {0};

			result.type   = BREAK_STMT;
			result.as.brk = parse_break_stmt(lex);

			return result;
		} break;

		case CONTINUE: {
			stmt result = {0};

			result.type    = CONTINUE_STMT;
			result.as.cont = parse_continue_stmt(lex);

			return result;
		} break;

		case RETURN: {
			stmt result = {0};

			result.type       = RETURN_STMT;
			result.as.returnn = parse_return_stmt(mem, lex);

			return result;
		} break;

		case OPEN_BRACE: {
			stmt result = {0};

			result.type     = BLOCK_STMT;
			result.as.block = parse_block_stmt(mem, lex);

			return result;
		} break;

		case ID: {
			token* ahead = lexer_look_ahead(lex, 1);
			if (ahead->type == ASSIGN) {
				stmt result = {0};

				result.type        = VAR_ASSIGN_STMT;
				result.as.var_assi = parse_var_assign(mem, lex);

				return result;
			}
		} break;

		case VAR: {
			stmt result = {0};

			result.type        = VAR_DEFINITION_STMT;
			result.as.var_defi = parse_var_def(mem, lex);

			return result;
		} break;

		default: {
			assert(false && "unreachable!");
		} break;
	}

	stmt result = {0};

	result.as.exprr = parse_expr(mem, lex);
	result.type     = EXPRESSION_STMT;
	lexer_expect_token(lex, SEMICOLON);

	return result;
}

// var_common -> |
//               | var id: type
static var_def parse_var_common(lexer* lex) {
	var_def var_common = {0};

	lexer_expect_token(lex, VAR);

	token* var_token = lexer_expect_token(lex, ID);
	var_common.loc  = var_token->loc;
	var_common.name = var_token->value;

	// (TODO): check for variable redefinition.

	lexer_expect_token(lex, COLON);

	var_token = lexer_expect_token(lex, TYPE);
	var_common.type = convert_string_to_var_type(&var_token->value);

	return var_common;
}

// var_def -> |
//            | var_common ';'
//            | const var_common ';'
var_def parse_var_def(arena* mem, lexer* lex) {
	// check for const
	token* curr_token = lexer_get_current_token(lex);
	bool is_const     = false;
	if (curr_token->type == CONST) {
		lexer_eat_token(lex);

		is_const = true;
	}

	var_def var = parse_var_common(lex);
	if (is_const) {
		var.is_const = is_const;

		curr_token = lexer_get_current_token(lex);
		if (curr_token->type == ASSIGN) {
			lexer_eat_token(lex);

			var.is_initialized = true;
			var.rhs            = parse_expr(mem, lex);
		} else {
			lexer_report_error(&var.loc,
							   "uninitialized `const %s`",
                                var.name.data);
		}
	} else {
		curr_token = lexer_get_current_token(lex);
		if (curr_token->type == ASSIGN) {
			lexer_eat_token(lex);

			var.is_initialized = true;
			var.rhs            = parse_expr(mem, lex);
		}
	}

	lexer_expect_token(lex, SEMICOLON);

	return var;
}

// param -> |
//          | ID ':' TYPE
//          | (TODO):  ||
//          |          ||
//          |         \  /
//          |          \/
//          | 'const' ID ':' TYPE
static func_param parse_func_param(lexer* lex) {
	func_param param = {0};

	token* curr_token = lexer_expect_token(lex, ID);
	param.loc = curr_token->loc;
	param.name= curr_token->value;

	lexer_expect_token(lex, COLON);
	curr_token = lexer_expect_token(lex, TYPE);

	param.type = convert_string_to_var_type(&curr_token->value);

	return param;
}

// func_decl -> |
//              | 'func' ID param_list ':' TYPE block_stmt
// param_lis -> |
//              | '(' param [',' param]* ')'
func_decl parse_func_decl(arena* mem, lexer* lex) {
	func_decl func = {0};

	lexer_expect_token(lex, FUNC);

	token* func_token = lexer_expect_token(lex, ID);
	func.loc        = func_token->loc;
	func.name       = func_token->value;
	func.parameters = list_create(sizeof(func_param));

	lexer_expect_token(lex, OPEN_PARENTHESIS);

	token* curr_token = lexer_get_current_token(lex);
	if (curr_token->type != CLOSE_PARENTHESIS) {

		func_param param = parse_func_param(lex);
		list_push_back(mem, &func.parameters, (void*) &param);

		curr_token = lexer_get_current_token(lex);
		while (curr_token->type == COMMA) {
			lexer_eat_token(lex);

			param = parse_func_param(lex);
			list_push_back(mem, &func.parameters, (void*) &param);

			curr_token = lexer_get_current_token(lex);
		}
	}

	lexer_expect_token(lex, CLOSE_PARENTHESIS);

	// if TYPE is ommited then the return type is void.
	curr_token = lexer_get_current_token(lex);
	if (curr_token->type == OPEN_BRACE) {
		func.return_type = VOID_VAR;
	} else {
		lexer_expect_token(lex, COLON);

		func_token        = lexer_expect_token(lex, TYPE);
		func.return_type  = convert_string_to_var_type(&func_token->value);
	}

	func.body = parse_block_stmt(mem, lex);

	return func;
}

// module -> |
//           | var_def
//           | func_def
module parse_module(arena* mem, lexer* lex) {
	module root  = {0};
	root.modules = list_create(sizeof(top_level));

	while (true) {
		token* curr_token = lexer_get_current_token(lex);
		if (curr_token->type == EOFF) {
			break;
		}

		switch (curr_token->type) {
			case CONST:
			case VAR: {
				top_level var_def = {0};

				var_def.type   = VAR_DEF_TL;
				var_def.as.var = parse_var_def(mem, lex);

				list_push_back(mem, &root.modules, (void *) &var_def);
			} break;

			case FUNC: {
				top_level func_decl = {0};

				func_decl.type    = FUN_DECL_TL;
				func_decl.as.func = parse_func_decl(mem, lex);

				list_push_back(mem, &root.modules, (void *) &func_decl);
			} break;

			default : {
				lexer_report_error(&curr_token->loc,
								   "expected top level variable or function defition/declaration and got `%s`.",
                                   curr_token->value.data);
			} break;
		}
	}

	return root;
}

// module
module parse(arena* mem, lexer* lex) {
	return parse_module(mem, lex);
}
