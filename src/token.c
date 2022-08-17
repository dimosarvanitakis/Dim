#include "token.h"

typedef struct tokens_literal {
	const char* text;
	token_type  type;
} tokens_literal;

// cache token strings for later use.
static tokens_literal token_literals[TOKEN_TYPES_COUNT] = {
	{"NONE"         ,   NONE               },
    {"EOFF"         ,   EOFF               },
	{"var" 	 	    ,	VAR                },
	{"const" 		,	CONST              },
	{"fn"           ,   FUNC               },
	{"if"			,	IF                 },
	{"else"		    ,	ELSE               },
	{"while"		,	WHILE              },
	{"do"			,	DO                 },
	{"return"		,   RETURN             },
	{"break"		, 	BREAK              },
	{"continue"	    , 	CONTINUE           },
	{"true"		    , 	TRUE	           },
	{"false"		, 	FALSE              },
	{"nil" 		    ,	NIL                },
	{"["            ,   OPEN_BRACKET       },
	{"]"            ,   CLOSE_BRACKET      },
	{"{"            ,   OPEN_BRACE         },
	{"}"            ,   CLOSE_BRACE        },
    {"("            ,   OPEN_PARENTHESIS   },
    {")"            ,   CLOSE_PARENTHESIS  },
    {";"            ,   SEMICOLON          },
    {","            ,   COMMA              },
	{":"            ,   COLON              },
	{"="            ,   ASSIGN             },
	{"||"			, 	OR	               },
	{"&&"			, 	AND	               },
	{"=="           ,   EQUALS             },
	{"!="           ,   NOT_EQUALS         },
	{">"            ,   GREATER            },
	{">="           ,   GREATER_EQUAL      },
	{"<"            ,   LESS               },
	{"<="           ,   LESS_EQUAL         },
	{"+"            ,   PLUS               },
	{"-"            ,   MINUS              },
	{"*"            ,   MULTI              },
	{"/"            ,   DIV                },
	{"%"            ,   MOD                },
	{"!"			,	NOT	               },
	{"++"           ,   INCREMENT          },
	{"--"           ,   DECREMENT          },
	{"#"            ,   HASH               },
	{"id"           ,   ID                 },
	{"type"         ,   TYPE               },
	{"integer"      ,   INTEGER            },
	{"real"         ,   REAL               },
	{"string"       ,   STRING             }
};

location location_create(uint32_t line, uint32_t column, const char *file) {
	location result;

	result.line		   = line;
	result.column	   = column;
	result.prev_column = column;
	result.file        = file;

	return result;
}

token* token_create(memory_arena* arena, token_type type, location loc, const char* value) {
	token* result = arena_allocate(arena, sizeof(token));

	result->type  = type;
	result->loc   = loc;
	result->value = string_create(arena, value);

	return result;
}

token* token_create_from_string(memory_arena* arena, token_type type, location loc, string value) {
	token* result = arena_allocate(arena, sizeof(token));

	result->type = type;
	result->loc	 = loc;

	result->value.length = value.length;
	result->value.data   = value.data;

	return result;
}

bool token_is_keyword(token* tok) {
	 token_type type = tok->type;

	return (type >= VAR &&
			type <= NIL) || type == TYPE;
}

bool token_is_unary_operator(token* token) {
	switch (token->type) {
		case NOT:
		case MINUS:
		case INCREMENT:
		case DECREMENT:
			return true;
		default:
			return false;
	}
}

bool token_is_binary_operator(token* token) {
	switch (token->type) {
		case OR:
		case AND:
		case EQUALS:
		case NOT_EQUALS:
		case GREATER:
		case GREATER_EQUAL:
		case LESS:
		case LESS_EQUAL:
		case PLUS:
		case MINUS:
		case MULTI:
		case DIV:
		case MOD:
			return true;
		default:
			return false;
	}
}

const char* token_get_type_to_string(token_type type) {
	return token_literals[type].text;
}

token_type is_keyword(string* text) {
	for (size_t key = 0; key < NIL; key++) {
		if (!strcmp(token_literals[key].text, text->data)) {
			return token_literals[key].type;
		}
	}

	if (is_primitive_type(text)) {
		return TYPE;
	}

	return NONE;
}

bool is_primitive_type(string* text) {
	const char* type = (const char*) text->data;

	if (!strcmp(type, "u8")   ||
        !strcmp(type, "u16")  ||
        !strcmp(type, "u32")  ||
		!strcmp(type, "u64")  ||
		!strcmp(type, "i8")   ||
		!strcmp(type, "i16")  ||
		!strcmp(type, "i32")  ||
		!strcmp(type, "i64")  ||
		!strcmp(type, "f32")  ||
		!strcmp(type, "f64")  ||
		!strcmp(type, "bool") ||
		!strcmp(type, "str")  ||
        !strcmp(type, "ptr")  ||
        !strcmp(type, "void")) {
		return true;
	}

	return false;
}
