#ifndef AST_H_
#define AST_H_

#include "token.h"
#include "common.h"

// Expr
typedef struct literal_expr   literal_expr;
typedef struct func_call_expr func_call_expr;
typedef struct lvalue_expr    lvalue_expr;
typedef struct binary_op_expr binary_op_expr;
typedef struct expr           expr;

// Stmt
typedef struct if_stmt       if_stmt;
typedef struct while_stmt    while_stmt;
typedef struct do_stmt       do_stmt;
typedef struct return_stmt   return_stmt;
typedef struct break_stmt    break_stmt;
typedef struct continue_stmt continue_stmt;
typedef struct block_stmt    block_stmt;
typedef struct var_assign    var_assign;
typedef struct stmt          stmt;

// Top-level ~ Module
typedef struct var_def    var_def;
typedef struct func_param func_param;
typedef struct func_decl  func_decl;
typedef struct top_level  top_level;
typedef struct module     module;

typedef enum literal_type {
    INTEGER_LIT,
    DOUBLE_LIT,
    BOOL_LIT,
    STRING_LIT
} literal_type;
struct literal_expr {
	literal_type type;

	union {
		int64_t		int_val;
		double		double_val;
		bool		bool_val;
		const char* string_val;
	} as;
};

struct func_call_expr {
	string   name;
	location loc;
	list*    arguments;
};

struct lvalue_expr {
	location loc;
	string   name;
};

typedef enum expr_type {
    LITERAL_EXPR,
	FUNC_CALL_EXPR,
	LVALUE_EXPR,
	BINARY_OP_EXPR,

	EXPR_TYPES_COUNT
} expr_type;
struct expr {
	expr_type type;
	location  loc;

	union {
		literal_expr    literal;
		func_call_expr  func_call;
		lvalue_expr     lvalue;
		binary_op_expr* binary_op;
	} as;
};

typedef enum binary_op_type {
     OR_BINARY_OP = 25,
	 AND_BINARY_OP,
     EQUALS_BINARY_OP,
     NOT_EQUALS_BINARY_OP,
     GREATER_BINARY_OP,
     GREATER_EQUAL_BINARY_OP,
     LESS_BINARY_OP,
     LESS_EQUAL_BINARY_OP,
     PLUS_BINARY_OP,
     MINUS_BINARY_OP,
     MULTI_BINARY_OP,
     DIV_BINARY_OP,
     MOD_BINARY_OP,

	 BINARY_OP_TYPES_COUNT
} binary_op_type;
struct binary_op_expr {
	binary_op_type	type;
	location		loc;
	expr			lhs;
	expr			rhs;
};

//(HACK): if block_strm -> stmt then we can create something like this
//        while(i<5) i++;
//        a while statement with body of just a sigle statement.

struct block_stmt {
	list* stmts;
};

struct if_stmt {
	expr         condition;
	block_stmt   then_body;
	bool         has_else;
	block_stmt   else_body;
	location     loc;
};

struct while_stmt {
	expr		condition;
	block_stmt	body;
	location	loc;
};

struct do_stmt {
	block_stmt  body;
	expr        condition;
	location    loc;
};

struct return_stmt {
	bool     has_result;
	expr     result;
	location loc;
};

struct break_stmt {
	location loc;
};

struct continue_stmt {
	location loc;
};

struct var_assign {
	string    name;
	location  loc;
	expr      value;
};

typedef enum var_type {
    U8_VAR = 0,
	U16_VAR,
	U32_VAR,
	U64_VAR,
	I8_VAR,
	I16_VAR,
	I32_VAR,
	I64_VAR,
	F32_VAR,
	F64_VAR,
	BOOL_VAR,
	STR_VAR,
	PTR_VAR,
	VOID_VAR,
	NONE_TYPE,

	VAR_TYPES_COUNT
} var_type;
struct var_def {
	location loc;
	bool     is_const;
	bool     is_initialized;
	string   name;
	var_type type;
	expr     rhs;
};

typedef enum stmt_type {
	IF_STMT,
    WHILE_STMT,
    DO_STMT,
	BREAK_STMT,
	CONTINUE_STMT,
    RETURN_STMT,
	BLOCK_STMT,
	VAR_ASSIGN_STMT,
	VAR_DEFINITION_STMT,
	EXPRESSION_STMT,

	STMT_TYPES_COUNT
} stmt_type;
struct stmt {
	stmt_type type;

	union {
		if_stmt		    iff;
		while_stmt		whilee;
		do_stmt		    doo;
		break_stmt		brk;
		continue_stmt	cont;
		return_stmt     returnn;
		block_stmt		block;
		var_assign      var_assi;
		var_def         var_defi;
		expr		    exprr;
	} as;
};

struct func_param {
	string   name;
	var_type type;
	location loc;
};

struct func_decl {
	location	loc;
	string		name;
	list*		parameters;
	var_type	return_type;
	block_stmt  body;
};

typedef enum top_level_type {
	VAR_DEF_TL = 0,
	FUN_DECL_TL
} top_level_type;
struct top_level {
	top_level_type type;

	union {
		var_def   var;
		func_decl func;
	} as;
};

struct module {
	list* modules;
};

#endif // !AST_H_
