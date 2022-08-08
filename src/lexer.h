#ifndef LEXER_H_
#define LEXER_H_

#include "token.h"

#define MAX_TOKEN_BUFFER 5

typedef struct tok_buffer tok_buffer;
typedef struct lexer      lexer;

struct tok_buffer {
	token*   tokens[MAX_TOKEN_BUFFER];
	uint32_t pos;
};

struct lexer {
	const char*		file;
	buffer			buff;
	memory_arena*	arena;
	location		curr_pos;
	uint32_t		position;
	tok_buffer		token_buffer;
};

lexer   lexer_create(memory_arena* arena, const char* file);
void    lexer_report_error(location* loc, const char* error, ...);

token* lexer_get_current_token(lexer* lex);
token* lexer_eat_token(lexer* lex);
token* lexer_expect_token(lexer* lex, token_type expected_type);
token* lexer_look_ahead(lexer* lex, uint32_t offset);

#endif // !LEXER_H_
