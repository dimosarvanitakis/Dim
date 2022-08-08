#ifndef PARSER_H_
#define PARSER_H_

#include "ast.h"
#include "lexer.h"

expr          parse_expr(memory_arena* arena, lexer* lex);

if_stmt       parse_if_stmt(memory_arena* arena, lexer* lex);
while_stmt    parse_while_stmt(memory_arena* arena, lexer* lex);
break_stmt    parse_break_stmt(lexer* lex);
continue_stmt parse_continue_stmt(lexer* lex);
return_stmt   parse_return_stmt(memory_arena* arena, lexer* lex);
block_stmt    parse_block_stmt(memory_arena* arena, lexer* lex);
var_assign    parse_var_assign(memory_arena* arena, lexer* lex);
stmt          parse_stmt(memory_arena* arena, lexer* lex);

var_def       parse_var_def(memory_arena* arena, lexer* lex);
func_decl     parse_func_decl(memory_arena* arena, lexer* lex);

module        parse_module(memory_arena* arena, lexer* lex);
module        parse(memory_arena* arena, lexer* lex);

#endif // !PARSER_H_
