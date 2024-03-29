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
    OP_GET_LOCAL, // loads local variables
    OP_SET_LOCAL, // allows us to set the value of a local variable
    OP_GET_GLOBAL, // loads global variables
    OP_DEFINE_GLOBAL, // defining global variable operation
    OP_SET_GLOBAL, // allow us to reset the values of a variable
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_EQUAL, // == => deals with != b/c !( == )
    OP_BUILD_LIST, // building a list in the first place
    OP_INDEX_SUBSCR, // getting a value at an index
    OP_STORE_SUBSCR, // storing at an index
    OP_GREATER, // < => deals with >= b/c is !( < )
    OP_LESS, // > => deals with <= b/c is !( > )
    OP_ADD, // addition operator
    OP_SUBTRACT, // subtraction operator
    OP_MULTIPLY, // multiplication operator
    OP_DIVIDE, // division operator
    OP_NOT, // NEGATION OF BOOLEANS
    OP_NEGATE, // negation operation (-)
    OP_PRINT, // printing operation,
    OP_JUMP, // a regulation unconditional branch (jump)
    OP_JUMP_IF_FALSE, // jump instruction for conditionals
    OP_LOOP, // loop isntruction
    //OP_SCAN, // scanning operation
    OP_CALL, // function calls
    OP_CLOSURE, // closures
    OP_CLOSE_UPVALUE, // for closing upvalues out
    OP_RETURN, // return from current instruction
    OP_CLASS, // class operation
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