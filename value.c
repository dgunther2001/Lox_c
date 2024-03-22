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
    printf("%g", value); // just straight up prints the passed value out to the user
}