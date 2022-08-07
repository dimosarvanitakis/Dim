#include "arena.h"

region* region_create(int32_t capacity) {
  region* result;

  result = (region*) malloc(sizeof(region));
  result->capacity = capacity;
  result->size     = 0;
  result->buffer   = (char*) malloc(capacity * sizeof(char));
  result->next     = NULL;

  memset(result->buffer,'\0', capacity);

  return result;
}

void region_clear(region* reg) {
  // Now we can overate the region
  reg->size = 0;
}

void region_free(region* reg) {
  free(reg->buffer);
  free(reg);

  reg = NULL;
}

void region_tell_stats(region* reg) {
    fprintf(stdout, "\t[region] [Mem:%p] [Size:%u] [Capacity:%u] [Free:%.2f]\n",
            reg->buffer,
            reg->size,
            reg->capacity,
            (float)((float)(reg->capacity - reg->size) / (float)reg->capacity) * 100);
}

arena arena_create(const char *id) {
  arena mem;

  mem.id   = id;
  mem.head = NULL;
  mem.last = NULL;

  return mem;
}

// TODO: Add alignment
void* arena_allocate(arena* mem, int32_t size) {
    // first time using the memory mem. Allocate the first region.
    if (mem->head == NULL &&
        mem->last == NULL) {
      int32_t new_region_size = (size > DEFAULT_ARENA_SIZE ? size : DEFAULT_ARENA_SIZE);
      region* new_region      = region_create(new_region_size);

      mem->head = new_region;
      mem->last = new_region;
    }

    // Begin the search to which we allocate.
    region*  curr   = mem->last;
    bool     alloc  = false;
    char*    memory = NULL;

    while (!alloc) {
      memory = (char *) (curr->buffer + curr->size);
      int32_t r_size = (int32_t) ((memory + size) - (curr->buffer + curr->size));

      if ((curr->size + r_size) > curr->capacity) {

        // Search in the next available region.
        if (curr->next != NULL) {
          curr = curr->next;

          continue;
        // Not another region available we running out of memory.
        // Allocate a new region and allocate the memory in the next cicle.
        } else {
          int32_t new_region_size = (size > DEFAULT_ARENA_SIZE ? size : DEFAULT_ARENA_SIZE);
          region* new_region      = region_create(new_region_size);

          mem->last->next = new_region;
          mem->last       = new_region;
          curr            = mem->last;

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

// void PushregionToarena(arena* arena, region* region) {
//   //We can add it to the beggin of the list
//   region->next = arena->head;
//   arena->head  = region;
// }

void arena_clear(arena* mem) {
  for (region* reg = mem->head;
       reg != NULL;
       reg = reg->next) {
    region_clear(reg);
  }
}

void arena_free(arena* mem) {
    region* reg = mem->head;

    while (reg != NULL) {
        region* next = reg->next;

        region_free(reg);

        reg = next;
    }

    mem->head = NULL;
    mem->last = NULL;
}

void arena_inspect(arena* mem) {
    fprintf(stdout, "Arena Stats[%s]:\n", mem->id);

    for (region* reg = mem->head;
         reg != NULL;
         reg = reg->next) {
      region_tell_stats(reg);
    }
}
