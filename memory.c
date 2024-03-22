#include <stdlib.h>

#include "memory.h"

/*
PARAMS ->       oldSize         newSize         Operation       
                   0            Non-zero        Allocate a new block
                Non-zero           0            Deallocation?
                Non-zero        < oldSize       Shrink block of allocated memory
                No-zero         > oldSize       Grow block of allocated memory
*/
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if (newSize == 0) { // frees up the pointer and returns null if we are deallocatin
        free(pointer);
        return NULL;
    }

    // realloc is equivalent to malloc when newSize = 0, so this is the catchall for the other three cases not accounted for
    void* result = realloc(pointer, newSize); // reallocates a new pointer with a piece of reallocated memory that uses the initial pointer, but allocates the correct size
    if (result == NULL) exit(1); // if there isn't enough memory for realloc, we exit the program
    return result;
}