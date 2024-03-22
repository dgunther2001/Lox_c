#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2) // macro that calculates a new capacity based on the given capacity -> GROWTH FACTOR OF 2
    // goes straight to 8 elements if it's 0

#define GROW_ARRAY(type, pointer, oldCount, newCount) (type*)reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * newCount) // essentially calls the reallocate function where the real work gets done

#define FREE_ARRAY(type, pointer, oldCount) reallocate(pointer, sizeof(type) * oldCount, 0) // another "wrapper" that calls the realloc function, but sets the size of allocated memory to 0

void* reallocate(void* pointer, size_t oldSize, size_t newSize); // reallocates an array of a larger size, and will set the void pointer to the proper type

#endif