#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type) // extracts the object type from a given value

#define IS_STRING(value)    isObjType(value, OBJ_STRING)
#define IS_LIST(value)      isObjType(value, OBJ_LIST)

// back and forth between lox and c strings (array vs object representation)
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)
#define AS_LIST(value)      ((ObjList*)AS_OBJ(value))

typedef enum {
    OBJ_STRING,
    OBJ_LIST
} ObjType;

struct Obj {
    ObjType type; // defines the type
    struct Obj* next; // intrusive linked list that enables basic garbage collection, as we have reference to all objects
}; 

struct ObjString {
    Obj obj; // the object itself
    int length; // string length
    char* chars; // pointer to the actual string
    uint32_t hash; //caching the strings so we don't have to fully walk them each time we want to call the hash function on it (stores the hash code in the object itself)
};

typedef struct {
    Obj obj; //the object itself
    int count; // number of iterms currently
    int capacity; // list capacity
    Value* items; // pointer the items in the list
} ObjList;

ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

ObjList* newList();
void appendToList(ObjList* list, Value value);
void storeToList(ObjList* list, int index, Value value);
Value indexFromList(ObjList* list, int index);
void deleteFromList(ObjList* list, int index);
bool isValidListIndex(ObjList* list, int index);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif