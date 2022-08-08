#ifndef TOKEN_H_
#define TOKEN_H_

#include "common.h"

typedef struct token	token;
typedef struct location location;

typedef enum token_type {
	//Debug
	NONE = 0,
	EOFF,

	//Keywords
	VAR,
	CONST,
	FUNC,
	IF,
	ELSE,
	WHILE,
	DO,
	RETURN,
	BREAK,
	CONTINUE,
	TRUE,
	FALSE,
	NIL,

	//Punctuation
	OPEN_BRACKET,
	CLOSE_BRACKET,
	OPEN_BRACE,
	CLOSE_BRACE,
	OPEN_PARENTHESIS,
	CLOSE_PARENTHESIS,
	SEMICOLON,
	COMMA,
	COLON,

	//Operators
	ASSIGN,
	OR,
	AND,
	EQUALS,
	NOT_EQUALS,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,
	PLUS,
	MINUS,
	MULTI,
	DIV,
	MOD,
	NOT,
	INCREMENT,
	DECREMENT,
	HASH,

	//Ids and literals
	ID,
	TYPE,
	INTEGER,
	REAL,
	STRING,

	//Count
	TOKEN_TYPES_COUNT
} token_type;

struct location {
    uint32_t	line;
    uint32_t	column;
    uint32_t	prev_column;
    const char* file;
};

struct token {
	token_type 	type;
	location    loc;
	string      value;
};

location    create_location(uint32_t line, uint32_t column, const char* file);

token*      create_token(memory_arena* arena, token_type type, location loc, const char* value);
token*      create_token_from_string(memory_arena* arena, token_type type, location loc, string value);

token_type  is_keyword(string* text);
bool        is_primitive_type(string* text);

bool        is_token_keyword(token* token);
bool        is_token_unary_operator(token* token);
bool        is_token_binary_operator(token* token);

const char* get_token_type_to_string(token_type type);

#endif // !TOKEN_H_
