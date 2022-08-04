#include "lexer.h"

#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>

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

static token* read_number(lexer* lex, int32_t pos) {
	char*	from      = &lex->buff.data[pos];
	char*	until     = from;
	char	prev_char = *from;

	bool	is_real   = false;
	char	c		  = get_next_char(lex);

	if (isdigit(c) && prev_char == '0') {
		lexer_report_error(&lex->curr_pos, "numbers cannot start with 0[0-9].");
	}

	while (true) {

		if (c == '.') {
			if (is_real) break;
			is_real = true;
		} else if (!isdigit(c)) {
			break;
		}

		prev_char = c;
		c        = get_next_char(lex);

		until++;
	}

	retract(lex, c);
	until++;

	if (prev_char == '.') {
		is_real = false;
		retract(lex, prev_char);

		/* for (char* it = from; it != until; ++it) {
			if (*it != '.') {
				number.data[number.length] = *it;
				number.length++;
			}
		} */
	}

	return create_token_from_string(lex->mem,
								    is_real ? REAL : INTEGER,
								    lex->curr_pos,
								    string_create_from(lex->mem, from, (int32_t) (until - from)));
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
	str.data   = arena_allocate(lex->mem, str.length + 1);
	memcpy(str.data, buffer, str.length);
	str.data[str.length] = '\0';

	return create_token_from_string(lex->mem, STRING, lex->curr_pos, str);
}

static token* get_next_token(lexer* lex) {
	arena* mem = lex->mem;

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
				return create_token(mem, OPEN_BRACE, curr_pos, "{");
			} break;

			case '}': {
				return create_token(mem, CLOSE_BRACE, curr_pos, "}");
			} break;

			case '[': {
				return create_token(mem, OPEN_BRACKET, curr_pos, "]");
			} break;

		    case ']': {
				return create_token(mem, CLOSE_BRACKET, curr_pos, "[");
			} break;

			case '(': {
				return create_token(mem, OPEN_PARENTHESIS, curr_pos, "(");
			} break;

			case ')': {
				return create_token(mem, CLOSE_PARENTHESIS, curr_pos, ")");
			}break;

			case ',': {
				return create_token(mem, COMMA, curr_pos, ",");
			} break;

			case ':': {
				return create_token(mem, COLON, curr_pos, ":");
			} break;

			case ';': {
				return create_token(mem, SEMICOLON, curr_pos, ";");
			} break;

			//Operators
			case '=': {
				input = get_next_char(lex);
				if(input == '=')
					return create_token(mem, EQUALS, curr_pos, "==");
				else {
					retract(lex, input);
					return create_token(mem, ASSIGN, curr_pos, "=");
				}
			} break;

			case '+': {
				input = get_next_char(lex);
				if (input == '+')
					return create_token(mem, INCREMENT, curr_pos, "++");
				else {
					retract(lex, input);
					return create_token(mem, PLUS, curr_pos, "+");
				}
			} break;

			case '-': {
				input = get_next_char(lex);
				if (input == '-')
					return create_token(mem, DECREMENT, curr_pos, "--");
				else {
					retract(lex, input);
					return create_token(mem, MINUS, curr_pos, "-");
				}
			}

			case '*': return create_token(mem, MULTI, curr_pos, "*");
			case '/': return create_token(mem, DIV, curr_pos, "/");
			case '%': return create_token(mem, MOD, curr_pos , "%");

			// TODO: bitwise operators
			case '&': {
				input = get_next_char(lex);
				if(input == '&')
					return create_token(mem, AND, curr_pos, "&&");
				else {
					retract(lex, input);
					lexer_report_error(&lex->curr_pos, "unknown character `%c` after &.", input);
				}
			} break;

			case '|':{
				input = get_next_char(lex);
				if(input == '|')
					return create_token(mem, OR, curr_pos, "||");
				else {
					retract(lex, input);
					lexer_report_error(&lex->curr_pos, "unknown character `%c` after |.", input);
				}
			} break;

			case '!': {
				input = get_next_char(lex);
				if(input == '=')
					return create_token(mem, NOT_EQUALS, curr_pos, "!=");
				else {
					retract(lex, input);
					return create_token(mem, NOT, curr_pos, "!");
				}
			} break;

			case '>': {
				input = get_next_char(lex);
				if(input == '=')
					return create_token(mem, GREATER_EQUAL, curr_pos, ">=");
				else {
					retract(lex, input);
					return create_token(mem, GREATER, curr_pos, ">");
				}
			} break;

			case '<': {
				input = get_next_char(lex);
				if(input == '=')
					return create_token(mem, LESS_EQUAL, curr_pos, "<");
				else {
					retract(lex, input);
					return create_token(mem, LESS, curr_pos, "<=");
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
				int32_t a_pos = lex->position - 1;

				if (isdigit(input)) {
					return read_number(lex, a_pos);
				}

				if (!isalpha(input)) {
					lexer_report_error(&lex->curr_pos, "unrecognised character %c.", input);
				}

				//Id or Keyword
				char* from  = &lex->buff.data[a_pos];
				char* until = from;
				while (isalnum(input = get_next_char(lex))) {
					until++;
				}

				// Retract the last character that we read
				retract(lex, input);
				until++;

				string key_or_id = string_create_from(lex->mem, from, (int32_t) (until - from));
				token_type type = is_keyword(&key_or_id);
				if (type == NONE) {
					type = ID;
				}

				return create_token(mem, type, curr_pos, (const char*) key_or_id.data);
			} break;
		}
	}

	return create_token(mem, EOFF, lex->curr_pos, "eof");
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

	buff->data   = arena_allocate(lex->mem, sizeof(char) * size);
	buff->length = size;

	if (!fread(buff->data, size, 1, file_to_lex)) {
		fprintf(stderr, "Unable to read file %s.\n", lex->file);
		fclose(file_to_lex);
		exit(1);
	}

	fclose(file_to_lex);
}

lexer lexer_create(arena* mem, const char* file) {
	lexer lex;

	lex.file = file;
	lex.mem  = mem;

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
	for (int32_t it = 0; it < MAX_TOKEN_BUFFER; it++) {
		fprintf(stdout, "\t[%d] token_buffer[%d]= %s\n",
				lex->token_buffer.pos,
				it,
				get_token_type_to_string(lex->token_buffer.tokens[it]->type));
	}
}

// in case of refill we lost the last token
token* lexer_eat_token(lexer* lex) {
	token* curr_token = lexer_get_current_token(lex);

	//print_token_buffer(lex);

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
						   get_token_type_to_string(curr_token->type));
	}

	if (expected_token != curr_token->type) {
		lexer_report_error(&curr_token->loc,
						   "expected token %s and got `%s`.\n",
						   get_token_type_to_string(expected_token),
						   curr_token->value.data,
                           get_token_type_to_string(curr_token->type));
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
