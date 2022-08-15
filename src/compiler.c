#include "parser.h"
#include "llvm_generator.h"
#include "c_generator.h"

#include <unistd.h>
#include <getopt.h>

static void print_help() {
    fprintf(stdout,
                  "\n"
                  "Usage:\n"
                  "      alpha {options}\n");
    fprintf(stdout,
                  "Options:\n"
                  "        -f          File to compile.\n"
                  "        -c {file}   Complile to C code and output the code in a file.\n"
                  "        -h          Prints this help message.\n");
    exit(0);
}

static void generate_llvm_code(module* ast, const char* module_name) {
     llvm_generator generator = llvm_generator_create(module_name);

     emit_llvm_module(&generator, ast);

     fprintf(stdout, "\n Generated Code \n %s", LLVMPrintModuleToString(generator.module));
     char* errors = NULL;
     LLVMVerifyModule(generator.module, LLVMPrintMessageAction, &errors);
     LLVMDisposeMessage(errors);

     // if not errors occurred print the generated code.
     // if (!code.errors) {
     // } else {
     //    fprintf(stderr, "Compilation error(s). Aborting.\n");
     // }

     llvm_generator_clear(&generator);
}

static void generate_c_code (module* ast, const char* to_write) {
    c_generator generator = c_generator_create(to_write);

    emit_c_module(&generator, ast);
}

int main(int argc, char* argv[]) {
    char* to_compile  = NULL;
    bool  c_code      = false;
    char* c_code_file = NULL;

    memory_arena* arena  = arena_create("lexer_parser_arena");

    if (argc <= 1) {
        print_help();
    }

    int opt;
    while ((opt = getopt(argc, argv, "hf:c:")) != -1) {

        switch(opt) {
            case 'h': {
                print_help();
            } break;

            case 'f': {
                unsigned long size = strlen(optarg);
                to_compile = arena_allocate(arena, sizeof(char)* (size + 1));
                strcpy(to_compile, optarg);
            } break;

            case 'c': {
                unsigned long size = strlen(optarg);

                c_code      = true;
                c_code_file = arena_allocate(arena, sizeof(char) * (size + 1));
                strcpy(c_code_file, optarg);
            } break;

            case ':': {
                fprintf(stderr, "Option %c needs a value\n", optopt);
            } break;

            case '?': {
                fprintf(stderr, "Unknown option: %c\n", optopt);
                print_help();
            } break;
        }
    }

    if (!to_compile ||
       (c_code && !c_code_file))
    {
        fprintf(stderr, "Missing input file.\n");
        print_help();
    }

    lexer lex  = lexer_create(arena, (const char*) to_compile);

    module ast = {0};
    ast = parse(arena, &lex);

    if (!c_code) {
        generate_llvm_code(&ast, "dim");
    } else {
        generate_c_code(&ast, (const char*) c_code_file);
    }

    arena_inspect(arena);
    arena_free(arena);

    return 0;
}
