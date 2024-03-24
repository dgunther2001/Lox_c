#include <stdio.h>

#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) { // initializes an empty array that is our value pool
    array->values = NULL; // contains nothing
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if /* If we run out of space, we dynamically allocate more! */(array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value; // places the value into the next availible memory slot
    array->count++; // iterates the count
}

void freeValueArray(ValueArray* array) { // deallocates everything, and reinitializes the array to 0
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
    }
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false; // first a simple type check
    switch (a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL: return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        default: return false;
    }
}