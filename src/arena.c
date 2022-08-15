#include "arena.h"

memory_region* region_create(int32_t capacity) {
  memory_region* region;

  region = (memory_region*) malloc(sizeof(memory_region));
  region->capacity = capacity;
  region->size     = 0;
  region->buffer   = (char*) malloc(capacity * sizeof(char));
  region->next     = NULL;

  memset(region->buffer,'\0', capacity);

  return region;
}

void region_clear(memory_region* region) {
  // Now we can overate the region
  region->size = 0;
}

void region_free(memory_region* region) {
  free(region->buffer);
  free(region);

  region = NULL;
}

void region_tell_stats(memory_region* region) {
    fprintf(stdout, "\t[memory_region] [Mem:%p] [Size:%u] [Capacity:%u] [Free:%.2f%%]\n",
            region->buffer,
            region->size,
            region->capacity,
            (float)((float)(region->capacity - region->size) / (float)region->capacity) * 100);
}

memory_arena* arena_create(const char *id) {
  memory_arena* arena = (memory_arena*) malloc(sizeof(memory_arena));

  arena->id   = id;
  arena->head = NULL;
  arena->last = NULL;

  return arena;
}

// TODO: Add alignment
void* arena_allocate(memory_arena* arena, int32_t size) {
    // first time using the memory arena. Allocate the first memory_region.
    if (arena->head == NULL &&
        arena->last == NULL) {
      int32_t new_region_size = (size > DEFAULT_ARENA_SIZE ? size : DEFAULT_ARENA_SIZE);
      memory_region* new_region      = region_create(new_region_size);

      arena->head = new_region;
      arena->last = new_region;
    }

    // Begin the search to which we allocate.
    memory_region* curr = arena->last;
    bool  alloc  = false;
    char* memory = NULL;

    while (!alloc) {
      memory = (char *) (curr->buffer + curr->size);
      int32_t r_size = (int32_t) ((memory + size) - (curr->buffer + curr->size));

      if ((curr->size + r_size) > curr->capacity) {

        // Search in the next available memory_region.
        if (curr->next != NULL) {
          curr = curr->next;

          continue;
        // Not another region available we running out of memory.
        // Allocate a new region and allocate the memory in the next cicle.
        } else {
          int32_t new_region_size   = (size > DEFAULT_ARENA_SIZE ? size : DEFAULT_ARENA_SIZE);
          memory_region* new_region = region_create(new_region_size);

          arena->last->next = new_region;
          arena->last       = new_region;
          curr            = arena->last;

          continue;
        }

        // The memory is enougth for this allocation so just
        // return the memory.
      } else {
        memset(memory, '\0', size);

        curr->size += size;
        alloc       = true;
      }
    }

    return memory;
}

void arena_clear(memory_arena* arena) {
  for (memory_region* region = arena->head;
       region != NULL;
       region = region->next)
  {
    region_clear(region);
  }
}

void arena_free(memory_arena* arena) {
    memory_region* region = arena->head;

    while (region != NULL) {
        memory_region* next = region->next;

        region_free(region);

        region = next;
    }

    arena->head = NULL;
    arena->last = NULL;
}

void arena_inspect(memory_arena* arena) {
    fprintf(stdout, "Memory Arena Stats[%s]:\n", arena->id);

    for (memory_region* region = arena->head;
         region != NULL;
         region = region->next)
    {
      region_tell_stats(region);
    }
}
