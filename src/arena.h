#ifndef ARENA_H_
#define ARENA_H_

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#ifndef DEBUG
    #include <assert.h>
#else
    #define assert(cond)
#endif

#define KB                 1024
// in 4KB and less we have empty regions (because of the symbol table)
#define DEFAULT_ARENA_SIZE (8 * KB)

typedef struct region region;
typedef struct arena  arena;

struct region {
  region*     next;
  char*       buffer;
  int32_t     capacity;
  int32_t     size;
};

struct arena {
  region* head;
  region* last;
};

region* region_create(int32_t capacity);
void    region_clear(region* reg);
void    region_free(region* reg);
void    region_tell_stats(region* reg);

void*  arena_allocate(arena* mem, int32_t size);
void   arena_clear(arena* mem);
void   arena_free(arena* mem);
void   arena_inspect(arena* mem);

#endif // !ARENA_H_
