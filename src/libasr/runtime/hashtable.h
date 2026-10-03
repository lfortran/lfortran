#pragma once
#include <stdlib.h>
// Helper Macros (internal use)
#define _GV_CONCAT_IMPL(a, b) a##b
#define _GV_CONCAT(a, b) _GV_CONCAT_IMPL(a, b)

#define _GV_CONCAT3_IMPL(a, b, c) a##b##c
#define _GV_CONCAT3(a, b, c) _GV_CONCAT3_IMPL(a, b, c)
static const int EMPTY_BKT    = 0;
static const int OCCUPIED_BKT = 1;
static const int DELETED_BKT  = 2;

// Hashtable Definition Macro with Inlined Hash and Equality
#define DEFINE_HASHTABLE_FOR_TYPES(KEY_TYPE, VALUE_TYPE, NAME) \
    typedef struct _GV_CONCAT3(NAME, _, Entry) { \
        KEY_TYPE key; \
        VALUE_TYPE value; \
        int state; \
    } _GV_CONCAT3(NAME, _, Entry); \
    \
    typedef struct NAME { \
        _GV_CONCAT3(NAME, _, Entry) *buckets; \
        size_t num_buckets; \
        size_t size; \
    } NAME; \
    \
    static inline void _GV_CONCAT3(NAME, _, init)(NAME *ht, size_t initial_buckets) { \
        ht->num_buckets = initial_buckets; \
        ht->size = 0; \
        ht->buckets = (_GV_CONCAT3(NAME, _, Entry)*) malloc(sizeof(_GV_CONCAT3(NAME, _, Entry)) * initial_buckets); \
        for (size_t i = 0; i < initial_buckets; i++) { \
            ht->buckets[i].state = EMPTY_BKT; \
        } \
    } \
    \
    static inline void _GV_CONCAT3(NAME, _, insert)(NAME *ht, KEY_TYPE key, VALUE_TYPE value) { \
        if (ht->size >= 0.75 * ht->num_buckets) { \
            size_t new_num_buckets = ht->num_buckets * 2; \
            _GV_CONCAT3(NAME, _, Entry) *new_buckets = (_GV_CONCAT3(NAME, _, Entry)*) malloc(sizeof(_GV_CONCAT3(NAME, _, Entry)) * new_num_buckets); \
            for (size_t i = 0; i < new_num_buckets; i++) { \
                new_buckets[i].state = EMPTY_BKT; \
            } \
            for (size_t i = 0; i < ht->num_buckets; i++) { \
                if (ht->buckets[i].state == OCCUPIED_BKT) { \
                    KEY_TYPE existing_key = ht->buckets[i].key; \
                    VALUE_TYPE existing_value = ht->buckets[i].value; \
                    size_t hash_value = _GV_CONCAT3(NAME, _, HASH)(existing_key); \
                    size_t index = hash_value % new_num_buckets; \
                    while (new_buckets[index].state == OCCUPIED_BKT) { \
                        index = (index + 1) % new_num_buckets; \
                    } \
                    new_buckets[index].key = existing_key; \
                    new_buckets[index].value = existing_value; \
                    new_buckets[index].state = OCCUPIED_BKT; \
                } \
            } \
            free(ht->buckets); /*Free old buckets*/ \
            ht->buckets = new_buckets; \
            ht->num_buckets = new_num_buckets; \
        } \
        /*Insert into table -- Track deleted bucket to insert into if key not found*/\
        size_t hash_value = _GV_CONCAT3(NAME, _, HASH)(key); \
        size_t index = hash_value % ht->num_buckets; \
        size_t start_index = index; \
        size_t first_deleted = (size_t)-1; \
        while (ht->buckets[index].state != EMPTY_BKT) { \
            if (ht->buckets[index].state == OCCUPIED_BKT && \
                _GV_CONCAT3(NAME, _, EQUAL)(ht->buckets[index].key, key)) { \
                ht->buckets[index].value = value; \
                return; \
            } \
            if (ht->buckets[index].state == DELETED_BKT && \
                first_deleted == (size_t)-1) { \
                first_deleted = index; \
            } \
            index = (index + 1) % ht->num_buckets; \
            if (index == start_index) break; \
        } \
        if (first_deleted != (size_t)-1) { \
            index = first_deleted; \
        } \
        ht->buckets[index].key = key; \
        ht->buckets[index].value = value; \
        ht->buckets[index].state = OCCUPIED_BKT; \
        ht->size++; \
    } \
    \
    static inline VALUE_TYPE* _GV_CONCAT3(NAME, _, get)(NAME *ht, KEY_TYPE key) { \
        size_t hash_value = _GV_CONCAT3(NAME, _, HASH)(key); \
        size_t index = hash_value % ht->num_buckets; \
        size_t start_index = index; \
        while (ht->buckets && ht->buckets[index].state != EMPTY_BKT) { \
            if (ht->buckets[index].state == OCCUPIED_BKT && \
                _GV_CONCAT3(NAME, _, EQUAL)(ht->buckets[index].key, key)) { \
                return &ht->buckets[index].value; \
            } \
            index = (index + 1) % ht->num_buckets; \
            if (index == start_index) break; /*Probably won't happen*/ \
        } \
        return NULL; \
    }\
    \
    static inline void _GV_CONCAT3(NAME, _, remove)(NAME *ht, KEY_TYPE key){ \
        if (!ht->buckets || ht->num_buckets == 0) return; \
        size_t hash_value = _GV_CONCAT3(NAME, _, HASH)(key); \
        size_t index = hash_value % ht->num_buckets; \
        size_t start_index = index; \
        while (ht->buckets[index].state != EMPTY_BKT) { \
            if (ht->buckets[index].state == OCCUPIED_BKT && \
                _GV_CONCAT3(NAME, _, EQUAL)(ht->buckets[index].key, key)) { \
                ht->buckets[index].state = DELETED_BKT; \
                ht->size--; \
                return; \
            } \
            index = (index + 1) % ht->num_buckets; \
            if (index == start_index) break; \
        } \
    }
