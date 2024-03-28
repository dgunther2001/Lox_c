#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type) // extracts the object type from a given value

#define IS_CLOSURE(value)   isObjType(value, OBJ_CLOSURE);
#define IS_FUNCION(value)   isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)    isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)    isObjType(value, OBJ_STRING)
#define IS_LIST(value)      isObjType(value, OBJ_LIST)

// back and forth between lox and c strings (array vs object representation)
#define AS_CLOSURE(value)   ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)  ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value)    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)
#define AS_LIST(value)      ((ObjList*)AS_OBJ(value))

typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_LIST,
    OBJ_UPVALUE,
} ObjType;

struct Obj {
    ObjType type; // defines the type
    struct Obj* next; // intrusive linked list that enables basic garbage collection, as we have reference to all objects
}; 

typedef struct {
    Obj obj; // is just another object type, so it gets stored in the list of objects as well
    int arity; // the number of parameters we expect
    int upvalueCount; // the number of upvalues
    Chunk chunk; // each function has its own bytecode chunk
    ObjString* name; // the actual name of the function
} ObjFunction; // declares the function object data type

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj; // the object itself
    int length; // string length
    char* chars; // pointer to the actual string
    uint32_t hash; //caching the strings so we don't have to fully walk them each time we want to call the hash function on it (stores the hash code in the object itself)
};

typedef struct ObjUpvalue {
    Obj obj;
    Value* location;
    Value closed;
    struct ObjUpvalue* next; // intrusive linked lsit to store upvalues
} ObjUpvalue;

typedef struct {
    Obj obj;
    ObjFunction* function; 
    ObjUpvalue** upvalues; // pointer to another... POINTER (b/c upvalues are also dynamically allocated)
    int upvalueCount;
} ObjClosure;

typedef struct {
    Obj obj; //the object itself
    int count; // number of iterms currently
    int capacity; // list capacity
    Value* items; // pointer the items in the list
} ObjList;

ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjNative* newNative(NativeFn);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

ObjList* newList();
void appendToList(ObjList* list, Value value);
void storeToList(ObjList* list, int index, Value value);
Value indexFromList(ObjList* list, int index);
void deleteFromList(ObjList* list, int index);
bool isValidListIndex(ObjList* list, int index);
int listLength(ObjList* list);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif