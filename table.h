#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

// individual entries
typedef struct {
    ObjString* key; // uses a lox string as the key
    Value value; // stores a value that is indeterminate at compile time
} Entry;

// the hash table that will hold the key value pairs of variables
typedef struct {
    int count; // current number of currently stored key value pairs
    int capacity; // currentl;y allocated size of the array
    Entry* entries; // stores a pointer to an array of entrries
} Table;

void initTable(Table* table); // essentially the hash table constructor
void freeTable(Table * table); // deallocation of the hash table
bool tableGet(Table* table, ObjString* key, Value* value); // allows us to retrueve values from the hash table (boolean, so IS IT THERE???)
bool tableSet(Table* table, ObjString* key, Value value);  // putting entries into the hash table itself now
bool tableDelete(Table* table, ObjString* key); // allows us to delerer an entry from a hash table
void tableAddAll(Table* from, Table* to); // function to copy ALL entries of one hash table to another
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash); // allows us to return a pointer instead of duplicate entries, which we are trying to avoid with tableGet() & findEntry() methods
void tableRemoveWhite(Table* table);
void markTable(Table* table); // mark the global varibales in our hash table

#endif