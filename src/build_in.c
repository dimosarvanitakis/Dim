#include "build_in.h"

#include <stdio.h>
#include <stdarg.h>

EXPORT void print(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
}
