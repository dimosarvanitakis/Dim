#ifndef COMMON_H_
#define COMMON_H_

#include "arena.h"

#define MAX_PRINT_BUFFER 1024

typedef struct list_node list_node;
typedef list_node*       list_it;
typedef struct list      list;

typedef bool (*search_fn) (list_it, void*);
typedef void (*for_each_fn) (list_it, void*);

typedef struct buffer buffer;
typedef struct buffer string;

struct list_node {
    list_node* next;
    void*      data;
};

struct list {
    list_node*  front;
    list_node*  back;
    uint32_t    length;
};

struct buffer {
    uint32_t length;
    char*    data;
};

int32_t get_string_literal_length(const char* string);
string  string_create(memory_arena* arena, const char* data);
string  string_create_from(memory_arena* arena, const char* data, uint32_t length);

list*    list_create(memory_arena* arena);
bool     list_is_empty(list* li);
void     list_purge(list* li);
void     list_push_back(memory_arena* arena, list* li, void* data);
void*    list_pop_back(list* li);
list_it  list_front(list* li);
list_it  list_back(list* li);
bool     list_search(list* li,  void* key, search_fn fn);
void     list_for_each(list* li, for_each_fn fn, void* extra);

#endif // !COMMON_H_
