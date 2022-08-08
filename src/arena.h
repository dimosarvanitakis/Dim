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
#define DEFAULT_ARENA_SIZE (4 * KB)

typedef struct memory_region memory_region;
typedef struct memory_arena  memory_arena;

struct memory_region {
  memory_region* next;
  char*          buffer;
  int32_t        capacity;
  int32_t        size;
};

struct memory_arena {
  const char*    id;
  memory_region* head;
  memory_region* last;
};

memory_region* region_create(int32_t capacity);
void           region_clear(memory_region* region);
void           region_free(memory_region* region);
void           region_tell_stats(memory_region* region);

memory_arena* arena_create(const char* id);
void*         arena_allocate(memory_arena* arena, int32_t size);
void          arena_clear(memory_arena* arena);
void          arena_free(memory_arena* arena);
void          arena_inspect(memory_arena* arena);

#endif // !ARENA_H_
