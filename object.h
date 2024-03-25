#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type) // extracts the object type from a given value

#define IS_STRING(value)    isObjType(value, OBJ_STRING)

// back and forth between lox and c strings (array vs object representation)
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_STRING,
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

ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif