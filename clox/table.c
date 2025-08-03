#include <string.h>

#include "table.h"
#include "memory.h"
#include "object.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table)
{
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table *table)
{
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
    uint32_t index = key->hash % capacity;
    Entry *tombstone = NULL; // track the first tombstone

    for (;;) {
        Entry *entry = &entries[index];
        if (!entry->key) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                return tombstone ? tombstone : entry;
            } else {
                // We found a tombstone
                if (!tombstone) tombstone = entry;
            }
        } else if (entry->key == key) {
            return entry;
        }
        index = (index + 1) % capacity;
    }
}

static void adjustCapacity(Table *table, int capacity)
{
    Entry *entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // During table adjustment, tombstones are not copied,
    // so we need to reset table->count.
    table->count = 0;

    for (int i = 0; i < table->capacity; i++) {
        Entry *entry = &table->entries[i];
        if (!entry->key) continue;

        Entry *dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(Table *table, ObjString *key, Value value)
{
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry *entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // Increment table->count only when an actual empty entry is found.
    // Tombstones are treated as full entries, so they are already 
    // accounted for in table->count.
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool tableGet(Table *table, ObjString *key, Value *value)
{
    if (table->count == 0) return false;

    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (!entry->key) return false;

    *value = entry->value;
    return true;
}

void tableAddAll(Table *from, Table *to)
{
    for (int i = 0; i < from->capacity; i++) {
        Entry *entry = &from->entries[i];
        if (entry->key) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

bool tableDelete(Table *table, ObjString *key)
{
    if (table->count == 0) return false;

    // Find an entry.
    Entry *entry = findEntry(table->entries, table->capacity, key);
    if (!entry->key) return false;

    // Place a tombstone at the entry (key: NULL, value: true).
    // Treat the tombstone as a full entry, so there's no need to 
    // decrement table->count. The findEntry() function will break 
    // the loop when it encounters an empty entry. If we treat the 
    // tombstone as an empty entry, we may not find an actual empty 
    // entry, which could lead to an infinite loop. This is worse 
    // than allowing some extra entry overflow if we treat the 
    // tombstone as a full entry.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

ObjString *tableFindString(Table *table, const char *chars,
        int length, uint32_t hash)
{
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry *entry = &table->entries[index];
        if (!entry->key) {
            // Stop if we found an empty non-tombstone entry.
            // Skip tombstone (treat tombstone as full entry).
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length &&
                entry->key->hash == hash &&
                memcmp(entry->key->chars, chars, length) == 0) {
            // We found it.
            return entry->key;
        }
        index = (index + 1) % table->capacity;
    }
}

