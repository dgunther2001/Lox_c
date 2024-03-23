#ifndef clox_vm_h
#define clox_vm_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define STACK_MAX 256 // maximumm stack size

typedef struct {
    Chunk* chunk;
    uint8_t* ip; // points straight to the current bytecode isntruction in the chunk
    Value stack[STACK_MAX]; // array of values defined as the stack
    Value* stackTop; // THE STACK POINTERRRRR
} VM;

typedef enum { // vm runs the chunk & and responds with a value from this enum
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();



#endif