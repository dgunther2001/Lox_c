#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT, // constant production instruction
    OP_NIL, // NIL
    OP_TRUE, // TRUE
    OP_FALSE, // FALSE
    OP_POP, // popping instruction
    OP_GET_GLOBAL, // loads global variables
    OP_DEFINE_GLOBAL, // defining global variable operation
    OP_SET_GLOBAL, // allow us to reset the values of a variable
    OP_EQUAL, // == => deals with != b/c !( == )
    OP_GREATER, // < => deals with >= b/c is !( < )
    OP_LESS, // > => deals with <= b/c is !( > )
    OP_ADD, // addition operator
    OP_SUBTRACT, // subtraction operator
    OP_MULTIPLY, // multiplication operator
    OP_DIVIDE, // division operator
    OP_NOT, // NEGATION OF BOOLEANS
    OP_NEGATE, // negation operation (-)
    OP_PRINT, // printing operation
    OP_RETURN, // return from current instruction
} OpCode; // defines the opcode of our operation (note using bytecode instead of native assembly)

typedef struct { /* dynamic arrays are.. cache friendly; constant-time for indexed lookup; constant-time for appending */
    int count; // current number of elements in the array
    int capacity; // array capacity
    uint8_t* code;
    int* lines; // actual array of lines
    ValueArray constants; // actually stores some values
} Chunk; // chunk of 8-bit integer bytecode instructions ("bite" sized instructions)

void initChunk(Chunk* chunk); // declares the chunk initialization function we implement in chunk.c ... pass a pointer to initialize it
void freeChunk(Chunk* chunk); // this allows us to free up and deallocate pieces of memory
void writeChunk(Chunk* chunk, uint8_t byte, int line); // allows us to write a byte to our array of bytecode, and dynamically allocates more memory if count > capacity
int addConstant(Chunk* chunk, Value value); // allows us write to the value array in chunks

#endif