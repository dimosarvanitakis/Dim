#include "parser.h"
#include "codegen.h"

#include <unistd.h>
#include <getopt.h>

void print_help() {
    fprintf(stdout,
                  "\n"
                  "Usage:\n"
                  "      alpha {options}\n");
    fprintf(stdout,
                  "Options:\n"
                  "        -f    File to compile.\n"
                  "        -h    Prints this help message.\n");
    exit(0);
}

int main(int argc, char* argv[]) {
    char* file = NULL;
    arena mem  = {0};

    if (argc <= 1) {
        print_help();
    }

    int opt;
    while ((opt = getopt(argc, argv, "hf:")) != -1) {

        switch(opt) {
            case 'h': {
                print_help();
            } break;

            case 'f': {
                unsigned long size = strlen(optarg);
                file = arena_allocate(&mem, sizeof(char)* (size + 1));
                strcpy(file, optarg);
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

    if (file == NULL) {
        fprintf(stderr, "Missing file.\n");
        print_help();
    }

    lexer lex  = lexer_create(&mem, (const char*) file);

    module ast = {0};
    ast = parse(&mem, &lex);

    codegen code = codegen_create(&mem, file);

    codegen_generate_module(&code, &ast);

    fprintf(stdout, "\n Generated Code \n %s", LLVMPrintModuleToString(code.module));
    char* errors = NULL;
    LLVMVerifyModule(code.module, LLVMPrintMessageAction, &errors);
    LLVMDisposeMessage(errors);

    // if not errors occurred print the generated code.
    // if (!code.errors) {
    //} else {
    //    fprintf(stderr, "Compilation error(s). Aborting.\n");
    //}

    codegen_clear(&code);

    arena_inspect(&mem);
    arena_free(&mem);

    return 0;
}
