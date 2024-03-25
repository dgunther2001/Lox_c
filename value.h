#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum { // enumeration to allow us to compare types
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ // HEAP ALLOCATED DATA TYPES (pointer to the heap)
} ValueType;

typedef struct {
    ValueType type; // this is the type enum
    union { // allocates a "double's" worth of memory, but allows to overlap, as we can only actually have one type and thus value at a time
        bool boolean; 
        double number;
        Obj* obj;
    } as;
} Value;

/*
* these macros check a value type to make sure that we are working with the appropriate types
*/
#define IS_BOOL(value)      ((value).type == VAL_BOOL)
#define IS_NIL(value)       ((value).type == VAL_NIL)
#define IS_NUMBER(value)    ((value).type == VAL_NUMBER)
#define IS_OBJ(value)       ((value).type == VAL_OBJ)

/*
* takes a loc value and returns it back as a c value
*/
#define AS_OBJ(value)       ((value).as.obj)
#define AS_BOOL(value)      ((value).as.boolean)
#define AS_NUMBER(value)    ((value).as.number)


/*
* takes a c value, and produces a Lox value with the appropriate type tag, as well as the value itself
*/
#define BOOL_VAL(value)     ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL             ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)   ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)     ((Value){VAL_OBJ, {.obj = (Obj*)object}}) // takes an object pointer and wraps it in a value

typedef struct { // just a dynamic array of values that are larger than the byte chunks (very similar to how the JVM works)
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);

/*
* just declaring some needed functionality implemented in the actual c file
*/
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif