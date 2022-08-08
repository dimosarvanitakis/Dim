#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "common.h"

#define HASH_MULTIPLIER 65599
#define BUCKETS         509

typedef struct symbol_table        symbol_table;
typedef struct symbol_table_entry  symbol_table_entry;
typedef struct symbol_table_entry* entry_it;

typedef void (*symbol_table_apply)(const char *key, void *value, void *extra);
typedef bool (*symbol_table_search)(void* data, void* extra);

typedef enum symbol_table_code {
    SYM_ALLOCATION_FAILED,
    SYM_ALREADY_EXISTS,
    SYM_NOT_FOUND,
    SYM_SUCCESS,
} symbol_table_code;

struct symbol_table_entry {
  char                *key;
  void                *value;
  symbol_table_entry  *next;
};

struct symbol_table {
  uint32_t               length;
  symbol_table_entry   **table_head;
};

symbol_table*     symbol_table_create(memory_arena* arena);
uint32_t          symbol_table_get_length(symbol_table* osymbol_table);

symbol_table_code symbol_table_put(memory_arena* arena,
                                   symbol_table* osymbol_table, 
                                   const char *pc_key, 
                                   const void *pv_value);

symbol_table_code symbol_table_remove(symbol_table* osymbol_table, const char *pc_key);

symbol_table_code symbol_table_contains(symbol_table* osymbol_table, 
                                        const char *pc_key, 
                                        symbol_table_search extra_cond, 
                                        void* extra);

void*             symbol_table_get(symbol_table* osymbol_table, const char *pc_key);
void              symbol_table_map(symbol_table* osymbol_table,
                                   void (*pf_apply)(const char *pc_key, void *pv_value, void *pv_extra),
                                   const void *pv_extra);
#endif
