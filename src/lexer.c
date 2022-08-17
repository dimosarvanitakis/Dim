#include "lexer.h"

#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>

static inline bool is_decimeal (char c) {
	return (c == '.');
}

static inline bool is_hex(char c) {
	return (c == 'x');
}

static inline char get_current_char(lexer* lex) {
	return (char) lex->buff.data[lex->position - 1];
}

static char get_next_char(lexer* lex) {
	buffer*   buff     = &lex->buff;
	location* curr_pos = &lex->curr_pos;
	char c;

	if (lex->position >= buff->length)
		return EOF;

	c = (char) buff->data[lex->position];
	lex->position++;

	if (c == '\n') {
		curr_pos->line++;
		curr_pos->prev_column = curr_pos->column;
		curr_pos->column      = 0;
	} else {
		curr_pos->column++;
	}

	return c;
}

static void retract(lexer* lex, char c) {
	location* curr_pos = &lex->curr_pos;

	if (lex->position <= 0)
		return;

	if (c == '\n') {
		curr_pos->line--;
		curr_pos->column = curr_pos->prev_column;
	} else {
		curr_pos->column--;
	}

	lex->position--;
}

static void read_comment(lexer* lex) {
	char c;
	while ((c = get_next_char(lex)) != EOF) {
		if (c == '\n') {
			break;
		}
	}
}

static inline token* read_hex_number(lexer* lex) {
	char hex_number[128]   = {};
	int32_t hex_number_pos = 0;
	char* temp             = NULL;

	char c = get_next_char(lex);
	while((c >= '0' && c <= '9') ||
		  (c >= 'a' && c <= 'f') ||
		  (c >= 'A' && c <= 'F'))
	{
		hex_number[hex_number_pos++] = c;
		c = get_next_char(lex);
	}
	retract(lex, c);

	return token_create_integer(lex->arena,
								lex->curr_pos,
								strtoll(hex_number, &temp, 16));
}

static token* read_number(lexer* lex) {
	char*   temp        = NULL;
	char    number[128] = {};
	int32_t number_pos  = 0;
	char c              = get_current_char(lex);

	//if (isdigit(c) && prev_char == '0') {
	//	lexer_report_error(&lex->curr_pos, "numbers cannot start with 0[0-9].");
	//}
	number[number_pos++] = c;
	if (c == '0') {
		c = get_next_char(lex);
		if (is_hex(c)) {
			return read_hex_number(lex);
		} else {
			retract(lex, c);
		}
	}

	// read until the end of the number
	c = get_next_char(lex);
	while (isdigit(c)) {
		number[number_pos++] = c;
		c = get_next_char(lex);
	}

	if (is_decimeal(c)) {
		number[number_pos++] = c;
		c = get_next_char(lex);
		if (isdigit(c)) {
			number[number_pos++] = c;

			c = get_next_char(lex);
			while(isdigit(c)) {
				number[number_pos++] = c;
				c = get_next_char(lex);
			}

			retract(lex, c);

			return token_create_decimal(lex->arena,
										lex->curr_pos,
										strtod(number, &temp));
		} else {
			lexer_report_error(&lex->curr_pos, "unfinished decimal number.\n");
		}
	}
	retract(lex, c);

	return token_create_integer(lex->arena,
								lex->curr_pos,
								strtoll(number, &temp, 10));
}

static token* read_string(lexer* lex) {
	char    buffer[1024];
	int32_t buff_pos = 0;

	char c;
	while ((c = get_next_char(lex))) {

		if (c == EOF) {
			lexer_report_error(&lex->curr_pos, "unclosed string.");
		}

		if (c == '\"') {
			break;
		}

		if (c == '\\') {

			c = get_next_char(lex);

			if (c == EOF) {
				lexer_report_error(&lex->curr_pos, "unclosed string.");
			}

			if (c == 't') {
				c = '\t';
			} else if (c == 'n') {
				c = '\n';
			} else if (c == '\\') {
				c = '\\';
			} else if (c == 'b') {
				c = '\b';
			} else if (c == '\"') {
				c = '\"';
			} else {
				lexer_report_error(&lex->curr_pos, "%c after slash, sequence not recognized.", c);
			}
		}

		buffer[buff_pos] = c;
		buff_pos++;
	}

	string str = {0};
	str.length = buff_pos;
	str.data   = arena_allocate(lex->arena, str.length + 1);
	memcpy(str.data, buffer, str.length);
	str.data[str.length] = '\0';

	return token_create_from_string(lex->arena, STRING, lex->curr_pos, str);
}

static token* get_next_token(lexer* lex) {
	memory_arena* arena = lex->arena;

	char input;
	while ((input = get_next_char(lex)) != EOF) {
		location curr_pos = lex->curr_pos;

		switch (input) {
			//Skip whitespaces
			case ' '  :
			case '\t' :
			case '\n' :
			case '\r' : {
			} break;

			//Punctuations
			case '{': {
				return token_create(arena, OPEN_BRACE, curr_pos, "{");
			} break;

			case '}': {
				return token_create(arena, CLOSE_BRACE, curr_pos, "}");
			} break;

			case '[': {
				return token_create(arena, OPEN_BRACKET, curr_pos, "]");
			} break;

		    case ']': {
				return token_create(arena, CLOSE_BRACKET, curr_pos, "[");
			} break;

			case '(': {
				return token_create(arena, OPEN_PARENTHESIS, curr_pos, "(");
			} break;

			case ')': {
				return token_create(arena, CLOSE_PARENTHESIS, curr_pos, ")");
			}break;

			case ',': {
				return token_create(arena, COMMA, curr_pos, ",");
			} break;

			case ':': {
				return token_create(arena, COLON, curr_pos, ":");
			} break;

			case ';': {
				return token_create(arena, SEMICOLON, curr_pos, ";");
			} break;

			//Operators
			case '=': {
				input = get_next_char(lex);
				if(input == '=')
					return token_create(arena, EQUALS, curr_pos, "==");
				else {
					retract(lex, input);
					return token_create(arena, ASSIGN, curr_pos, "=");
				}
			} break;

			case '+': {
				input = get_next_char(lex);
				if (input == '+')
					return token_create(arena, INCREMENT, curr_pos, "++");
				else {
					retract(lex, input);
					return token_create(arena, PLUS, curr_pos, "+");
				}
			} break;

			case '-': {
				input = get_next_char(lex);
				if (input == '-')
					return token_create(arena, DECREMENT, curr_pos, "--");
				else {
					retract(lex, input);
					return token_create(arena, MINUS, curr_pos, "-");
				}
			}

			case '*': return token_create(arena, MULTI, curr_pos, "*");
			case '/': return token_create(arena, DIV, curr_pos, "/");
			case '%': return token_create(arena, MOD, curr_pos , "%");

			// TODO: bitwise operators
			case '&': {
				input = get_next_char(lex);
				if(input == '&')
					return token_create(arena, AND, curr_pos, "&&");
				else {
					retract(lex, input);
					lexer_report_error(&lex->curr_pos, "unknown character `%c` after &.", input);
				}
			} break;

			case '|':{
				input = get_next_char(lex);
				if(input == '|')
					return token_create(arena, OR, curr_pos, "||");
				else {
					retract(lex, input);
					lexer_report_error(&lex->curr_pos, "unknown character `%c` after |.", input);
				}
			} break;

			case '!': {
				input = get_next_char(lex);
				if(input == '=')
					return token_create(arena, NOT_EQUALS, curr_pos, "!=");
				else {
					retract(lex, input);
					return token_create(arena, NOT, curr_pos, "!");
				}
			} break;

			case '>': {
				input = get_next_char(lex);
				if(input == '=')
					return token_create(arena, GREATER_EQUAL, curr_pos, ">=");
				else {
					retract(lex, input);
					return token_create(arena, GREATER, curr_pos, ">");
				}
			} break;

			case '<': {
				input = get_next_char(lex);
				if(input == '=')
					return token_create(arena, LESS_EQUAL, curr_pos, "<");
				else {
					retract(lex, input);
					return token_create(arena, LESS, curr_pos, "<=");
				}
			} break;


			// Comment
			case '#': {
				read_comment(lex);
			} break;

			// Strings
			case '\"': {
				return read_string(lex);
			} break;

			// IDs, number etc.
			default: {
				char name[256]   = {};
				int32_t name_pos = 0;

				if (isdigit(input)) {
					return read_number(lex);
				}

				if (!isalpha(input)) {
					lexer_report_error(&lex->curr_pos, "unrecognised character %c.", input);
				}

				//Id or Keyword
				name[name_pos++] = input;
				while (isalnum(input = get_next_char(lex))) {
					name[name_pos++] = input;
				}
				// Retract the last character that we read
				retract(lex, input);

				string key_or_id = string_create(lex->arena, name);
				token_type type  = is_keyword(&key_or_id);
				if (type == NONE) {
					type = ID;
				}

				return token_create(arena, type, curr_pos, (const char*) key_or_id.data);
			} break;
		}
	}

	return token_create(arena, EOFF, lex->curr_pos, "eof");
}

static void refill_token_buffer(lexer* lex) {
	int32_t curr_token_pos = lex->token_buffer.pos;

	// Running out of token, fill the buffer.
	if (curr_token_pos + 1 == MAX_TOKEN_BUFFER) {

		lex->token_buffer.tokens[0] = lex->token_buffer.tokens[curr_token_pos];
		curr_token_pos = 1;
		for (; curr_token_pos < MAX_TOKEN_BUFFER; curr_token_pos++)
			lex->token_buffer.tokens[curr_token_pos] = get_next_token(lex);

		lex->token_buffer.pos = 0;
	}
}

static void lexer_load_file_to_buffer(lexer* lex) {
	FILE*	file_to_lex = NULL;
	buffer*	buff	    = &lex->buff;

	if (!(file_to_lex = fopen(lex->file, "r"))) {
		fprintf(stderr, "Unable to open file %s.\n", lex->file);
		exit(1);
	}

	fseek(file_to_lex, 0, SEEK_END);
	long size = ftell(file_to_lex);
	fseek(file_to_lex, 0, SEEK_SET);

	buff->data   = arena_allocate(lex->arena, sizeof(char) * size);
	buff->length = size;

	if (!fread(buff->data, size, 1, file_to_lex)) {
		fprintf(stderr, "Unable to read file %s.\n", lex->file);
		fclose(file_to_lex);
		exit(1);
	}

	fclose(file_to_lex);
}

lexer lexer_create(memory_arena* arena, const char* file) {
	lexer lex;

	lex.file  = file;
	lex.arena = arena;

	lex.curr_pos.line        = 1;
	lex.curr_pos.column      = 0;
	lex.curr_pos.prev_column = 0;
	lex.curr_pos.file        = file;

	lex.position	= 0;
	lex.buff.data   = NULL;
	lex.buff.length = 0;

	lex.token_buffer.pos = 0;
	memset(lex.token_buffer.tokens, 0, MAX_TOKEN_BUFFER * sizeof(token*));

    // load the file in the memory.
	lexer_load_file_to_buffer(&lex);

	// after that, cache the first tokens.
	for (int32_t it = 0; it < MAX_TOKEN_BUFFER; it++) {
		lex.token_buffer.tokens[it] = get_next_token(&lex);
	}

	return lex;
}

void lexer_report_error(location* loc, const char* error, ...) {
	fprintf(stderr, "%s:%d:%d ~ systax error: ",
			loc->file,
			loc->line,
			loc->column);

	va_list vargs;
	va_start(vargs, error);
	vfprintf(stdout, error, vargs);
	va_end(vargs);

	exit(1);
}

token* lexer_get_current_token(lexer* lex) {
	return lex->token_buffer.tokens[lex->token_buffer.pos];
}

// (FIXME): remove me
static void print_token_buffer(lexer* lex) {
	for(int32_t it = 0; it < MAX_TOKEN_BUFFER; it++) {
		token*    token = lex->token_buffer.tokens[it];
		fprintf(stdout, "\t[%d] token_buffer[%d]= %s ",
				lex->token_buffer.pos,
				it,
				token_get_type_to_string(token->type));
		if (token->type == INTEGER) {
			fprintf(stdout, "[%ld]\n", token->value.integer_value);
		} else if (token->type == DECIMAL) {
			fprintf(stdout, "[%f]\n", token->value.decimal_value);
		} else {
			fprintf(stdout, "[%s]\n", token->value.string_value.data);
		}
	}
}

// in case of refill we lost the last token
token* lexer_eat_token(lexer* lex) {
	token* curr_token = lexer_get_current_token(lex);

	print_token_buffer(lex);

	refill_token_buffer(lex);

	curr_token      = lex->token_buffer.tokens[lex->token_buffer.pos];
	lex->token_buffer.pos++;

	return curr_token;
}

token* lexer_expect_token(lexer* lex, token_type expected_token) {
	token* curr_token = lexer_eat_token(lex);

	if (curr_token->type == EOFF) {
		lexer_report_error(&curr_token->loc,
						   "expected token %s and got EOF.\n",
						   token_get_type_to_string(curr_token->type));
	}

	if (expected_token != curr_token->type) {
		lexer_report_error(&curr_token->loc,
						   "expected token %s and got `%s`.\n",
						   token_get_type_to_string(expected_token),
						   curr_token->value.string_value.data,
                           token_get_type_to_string(curr_token->type));
	}

	return curr_token;
}

token* lexer_look_ahead(lexer* lex, uint32_t offset) {
	assert(offset < MAX_TOKEN_BUFFER);

	// in case we want to look ahead in the end of the buffer
	refill_token_buffer(lex);

	// Out of bounds
	uint32_t ahead = lex->token_buffer.pos + offset;

	return lex->token_buffer.tokens[ahead];
}
