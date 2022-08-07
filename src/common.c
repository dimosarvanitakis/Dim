#include "common.h"

int32_t get_string_literal_length(const char* string) {
    int32_t length = 0;

    while (string[length] != '\0')
        ++length;

    return length;
}

string string_create(arena* mem, const char* data) {
    string result = {0};

    result.length = get_string_literal_length(data);
    result.data   = arena_allocate(mem, sizeof(char) * (result.length + 1));

    memcpy(result.data, data, result.length + 1);
    result.data[result.length] = '\0';

    return result;
}

string string_create_from(arena* mem, const char* data, uint32_t length) {
    string result = {0};

    result.length = length;
    result.data   = arena_allocate(mem, sizeof(char) * (length + 1));

    memcpy(result.data, data, result.length + 1);
    result.data[result.length] = '\0';

    return result;
}

list list_create(size_t element_size) {
    list li;

    li.front        = NULL;
    li.back         = NULL;
    li.length       = 0;
    li.element_size = element_size;

    return li;
}

bool list_is_empty(list* li) {
    return (!li->length);
}

void list_purge(list* li) {
    li->back   = NULL;
    li->front  = NULL;
    li->length = 0;
}

void list_push_back(arena* mem, list* li, void* data) {
    assert(mem);
    assert(li);

    list_node* node = arena_allocate(mem, sizeof(list_node));
    memset(node, 0, sizeof(list_node));

    node->data = arena_allocate(mem, li->element_size);
    node->next = NULL;
    memcpy(node->data, data, li->element_size);

    // Empty list
    if (li->front == NULL) {
        li->front = node;
        li->back  = node;
    } else {
        li->back->next = node;
        li->back       = node;
    }

    li->length++;
}

void* list_pop_back(list* li) {
    assert(li);

    if (list_is_empty(li))
        return NULL;

    void* data = NULL;
    if (li->front != li->back) {
        list_it it = li->front;
        while(it && it->next != li->back) {
            it = it->next;
        }

        data     = li->back->data;
        it->next = li->back->next;
        li->back = it;
    } else {
        data = li->back->data;

        li->front = NULL;
        li->back  = NULL;
    }

    li->length--;

    return data;
}

list_it list_front(list* li) {
    assert(li);

    return li->front;
}

list_it list_back(list* li) {
    assert(li);

    return li->back;
}

bool list_search(list* li, void* key, search_fn fn) {
    assert(li);
    assert(fn);

    list_it it = li->front;
    while(it != NULL) {

        if(fn(it, key)) {
            return true;
        }

        it = it->next;
    }

    return false;
}

void list_for_each(list *li, for_each_fn fn, void* extra) {
    assert(li);
    assert(fn);

    list_it it = li->front;
    while(it != NULL) {
        fn(it, extra);

        it = it->next;
    }
}
