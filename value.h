#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef double Value; // abstracts away our representations of values YAY!!!

typedef struct { // just a dynamic array of values that are larger than the byte chunks (very similar to how the JVM works)
    int capacity;
    int count;
    Value* values;
} ValueArray;

/*
* just declaring some needed functionality implemented in the actual c file
*/
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif