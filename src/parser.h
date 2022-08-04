#ifndef PARSER_H_
#define PARSER_H_

#include "ast.h"
#include "lexer.h"

expr          parse_expr(arena* mem, lexer* lex);

if_stmt       parse_if_stmt(arena* mem, lexer* lex);
while_stmt    parse_while_stmt(arena* mem, lexer* lex);
break_stmt    parse_break_stmt(lexer* lex);
continue_stmt parse_continue_stmt(lexer* lex);
return_stmt   parse_return_stmt(arena* mem, lexer* lex);
block_stmt    parse_block_stmt(arena* mem, lexer* lex);
var_assign    parse_var_assign(arena* mem, lexer* lex);
stmt          parse_stmt(arena* mem, lexer* lex);

var_def       parse_var_def(arena* mem, lexer* lex);
func_decl     parse_func_decl(arena* mem, lexer* lex);

module        parse_module(arena* mem, lexer* lex);
module        parse(arena* mem, lexer* lex);

#endif // !PARSER_H_
