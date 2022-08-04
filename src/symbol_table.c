#include "symbol_table.h"

static uint32_t symbol_table_hash(const char *key) {
  uint32_t ui;
  uint32_t ui_hash = 0U;
  for (ui = 0U; key[ui] != '\0'; ui++)
    ui_hash = ui_hash * HASH_MULTIPLIER + key[ui];
  return ui_hash;
}

symbol_table symbol_table_create(arena* mem) {
  symbol_table symtable = {0};

  symtable.table_head = (symbol_table_entry**)arena_allocate(mem, BUCKETS * sizeof(symbol_table_entry*));
  symtable.length     = 0U;

  return symtable;
}

uint32_t symbol_table_get_length(symbol_table* osymbol_table) {
  assert(osymbol_table != NULL);
  return osymbol_table->length;
}

symbol_table_code symbol_table_put(arena* mem, symbol_table* osymbol_table, const char *key , const void *pc_value) {
    uint32_t hashing;
    symbol_table_entry* new_entry;

    assert(osymbol_table);
    assert(key);

    if (symbol_table_contains(osymbol_table, key, NULL, 0) == SYM_SUCCESS) {
      return SYM_ALREADY_EXISTS;
    }

    hashing = symbol_table_hash(key) % BUCKETS;
    if ((new_entry = (symbol_table_entry*)arena_allocate(mem, sizeof(symbol_table_entry))) == NULL) {
      return SYM_ALLOCATION_FAILED;
    }

    if ((new_entry->key = (char*) arena_allocate(mem, strlen(key) + 1)) == NULL) {
      return SYM_ALLOCATION_FAILED;
    }

    new_entry->value = (void*) pc_value;
    strcpy(new_entry->key, key);
    new_entry->next = osymbol_table->table_head[hashing];

    osymbol_table->table_head[hashing] = new_entry;
    osymbol_table->length = osymbol_table->length + 1;

    return SYM_SUCCESS;
}

symbol_table_code symbol_table_remove(symbol_table* osymbol_table , const char *key){
    entry_it it;
    entry_it it_prev;
    uint32_t hash;

    assert(osymbol_table != NULL);
    assert(key != NULL);

    hash = symbol_table_hash(key) % BUCKETS;
    it   = osymbol_table->table_head[hash];

    if (strcmp(it->key, key) == 0) {
      osymbol_table->table_head[hash] = it->next;
      osymbol_table->length = osymbol_table->length - 1;

      return SYM_SUCCESS;
    }

    while (it != NULL && strcmp(it->key, key) != 0) {
      it_prev = it;
      it      = it->next;
    }

    if (it == NULL) return SYM_NOT_FOUND;

    it_prev->next = it->next;

    return SYM_SUCCESS;
}

// Search the table according to the given key and if specified
// alsp accordingly to extra conditions.
symbol_table_code symbol_table_contains(symbol_table* osymbol_table, 
                                        const char *key, 
                                        symbol_table_search extra_cond, 
                                        void* extra){
  entry_it it;

  assert(osymbol_table != NULL);
  assert(key != NULL);

  it = osymbol_table->table_head[symbol_table_hash(key) % BUCKETS];

  while (it != NULL) {
    if (strcmp(it->key, key) == 0) {
        
        if (extra_cond) {
            if(extra_cond(it->value, extra)) {
                return SYM_SUCCESS;
            } 
        } else {
            return SYM_SUCCESS;
        }
    }
    it = it->next;
  }

  return SYM_NOT_FOUND;
}

void* symbol_table_get(symbol_table* osymbol_table, const char *key) {
  entry_it it;

  assert(osymbol_table != NULL);
  assert(key != NULL);

  it = osymbol_table->table_head[symbol_table_hash(key) % BUCKETS];

  while (it != NULL) {
    if (strcmp(it->key, key) == 0) {
      return it->value;
    }

    it = it->next;
  }

  return NULL;
}

void symbol_table_map(symbol_table* osymbol_table,
                      symbol_table_apply apply,
                      const void *extra) {
  entry_it it;
  uint32_t bucket;

  assert(osymbol_table != NULL);
  assert(apply != NULL);

  for(bucket = 0U; bucket < BUCKETS; bucket++){
    it = osymbol_table->table_head[bucket];
    while(it != NULL){
      apply(it->key, (void*)it->value, (void*)extra);

      it = it->next;
    }
  }
}
