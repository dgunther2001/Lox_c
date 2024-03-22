#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) { /* initialized chunk to contain absolutely nothing */
    chunk->count = 0; // count is 0
    chunk->capacity = 0; // cannot yet store anything
    chunk->code = NULL; // contains nothing
    chunk->lines = NULL; // no lines thus far...
    initValueArray(&chunk->constants); // points to the array of constants
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity); // frees up the entire array
    FREE_ARRAY(int, chunk->lines, chunk->capacity); // frees up the lines array
    freeValueArray(&chunk->constants); // clears the values array when we deallocate it;
    initChunk(chunk); // intializes the chunk to size 0 again
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) { // writes to a chunk
    if /* if out array has no more capacity, do this */ (chunk->capacity < chunk->count + 1) { // if we want to write another instruction, but our array is too small
        int oldCapacity = chunk->capacity; // defines old capacity 
        chunk->capacity = GROW_CAPACITY(oldCapacity); // grows the capacity based on function in memory.c
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity); // grows the array based on function in memory.c
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity); // grows the lines array as well...
    }

    /*
    * this happens if our array does have enough capacity
    */
    chunk->code[chunk->count] = byte; // puts the instruction at the next empty location
    chunk->lines[chunk->count] = line; // iterates the line number too...
    chunk->count++; // iterates the count
}

int addConstant(Chunk* chunk, Value value) { // writes to the values array in a chunk
    writeValueArray(&chunk->constants, value); // writes the value to the value array in the chunk
    return chunk->constants.count-1; // returns the index of the constant in the array
}