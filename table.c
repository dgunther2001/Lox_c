#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75 // maximum load point of our hash table

void initTable(Table* table) { // initializes a completely empty hash table that takes up little to no memory
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity); // frees up the entry array
    initTable(table); // reinitializes the table
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) { // takes a key and an array of "buckets" and it figures out which bucket an entry belongs to
    uint32_t index = key->hash % capacity; // index is the hash of the key modulo the array capacity
    Entry* tombstone = NULL;
    for(;;) { // NOT INFINITE BECAUSE THERE IS ALWAYS SPACE IN OUR ARRAY
        Entry* entry = &entries[index];
        if (entry->key == NULL) { // if the key is NULL
            if (IS_NIL(entry->value)) { // we check if the entry value is null
                return tombstone != NULL ? tombstone : entry; // if tombstone is not null return a tombstone, otherwise return the entry
            } else {
                if (tombstone == NULL) tombstone = entry; // if the tombstone is null, set tombstone equal to entry
            }
        } else if (entry->key == key) { // just returns the entry given a valid key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false; // if the map is empty, return false

    Entry* entry = findEntry(table->entries, table->capacity, key); // otherwise, retrieve the entry
    if(entry->key == NULL) return false; // if the key is null, return false

    *value = entry->value; // puts the value in the passed parameter, so it creatively returns that as well
    return true; // returns true if the entry with the associated key exists
}

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity); // creates a bucket array with capacity entries
    for (int i = 0; i < capacity; i++) { // reinitializes because our hash value is based on array size, so we need to start from scratch
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }


    table->count = 0; // resets the count, so we don't count tombstones when reallocating
    for (int i = 0; i < table->capacity; i++) { // go back through the old array, and place in the new array based on the newly computed location values
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue; // iterates and goes back if we hit a tombstone or empty entry

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++; // iterates when we insert a non-tombstone
    }

    FREE_ARRAY(Entry, table->entries, table->capacity); // frees up the memory space from the old array
    table->entries = entries;
    table->capacity = capacity;


}

bool tableSet(Table* table, ObjString* key, Value value) { 
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) { // allocates new memory if we have run out of space and/or are too clustered (NEVER BECOMES COMPLETELY FULL)
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key); // figures out where we're supposed to end up after hashing
    bool isNewKey = entry->key == NULL; // checks to see if the key is unique?
    if (isNewKey && IS_NIL(entry->value) /* only increments the count if the bucket we are filling isn't a tombstone */ ) table->count++; 

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false; // if we have an empty table, we can't delete anything

    // this is where we actually find the entry in the table
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if(entry->key == NULL) return false;

    // here, we insert a tombstone so that we keep iterating after deletion (allows collision to exist but deletion to still be feasible)
    // tombstones are just a map entry with <NULL, true>, which is an invalid entry otherwise, so this is useful and valid
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) { // iterates over the entire from hash table
        Entry* entry = &from->entries[i]; // gets the particular hash table entry
        if (entry->key != NULL) { // if the key is not null, it puts the value into the to hash table
            tableSet(to, entry->key, entry->value);
        }
    }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL; // if the table is empty, return a null pointer

    uint32_t index = hash % table->capacity;
    for(;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) return NULL; // stop if we find an empty non tombstone entry
        } else if (entry->key->length == length && // check if the lengths match
                   entry->key->hash == hash && // check if the has values match
                   memcmp(entry->key->chars, chars, length) == 0 // check character by character
        ) return entry->key; // returns the value of the string (MORE LIKE A HASH SET => Strings are the keys)

        index = (index + 1) % table->capacity;
    }
}

void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key != NULL && !entry->key->obj.isMarked) {
            tableDelete(table, entry->key);
        }
    }
}

void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) { // mark each entry in the table of entries
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}