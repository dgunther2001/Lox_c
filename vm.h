#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT) // maximumm stack size

typedef struct { // REPRESENTS A SINGULAR ONGOING FUNCTION CALL!!!
    ObjClosure* closure;
    uint8_t* ip; // the caller stores its own instruction pointer => jump back to the ip in the frame calling this function
    Value* slots; // points at the first slot a function can use
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX]; // contains te instruction pointer we used to hold here => WE HAVE A MAXIMUM CALL DEPTH (no infinite recursion basically)
    int frameCount; // holds the current height of the CallFrame stack (how many ongoing function calls we have)

    Value stack[STACK_MAX]; // array of values defined as the stack
    Value* stackTop; // THE STACK POINTERRRRR
    Table globals; // hash table that stores our global variables
    Table strings; // a table of STRINGSSSSS!!!
    ObjUpvalue* openUpvalues; //head of the list of current upvalues
    size_t bytesAllocated;
    size_t nextGC;
    Obj* objects; // pointer to the head of the intrusive linked list of objects
    int grayCount;
    int grayCapacity;
    Obj** grayStack;
} VM;

typedef enum { // vm runs the chunk & and responds with a value from this enum
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm; // allows external exposure of our virtual machine

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();



#endif