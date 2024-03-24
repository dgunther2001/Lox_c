#include <stdio.h>

#include "debug.h"
#include "vm.h"
#include "compiler.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack; // just sets the stack pointer back to the base of the stack, which is just a pointer to an array anyways
}

void initVM() { // initializes the stack
    resetStack(); // just sets the stack pointer to 0
}

void freeVM() {

}

void push(Value value) { // pushes a value onto the stack
    *vm.stackTop = value; // dereferences the stacktop, and sets it equal to the value input
    vm.stackTop++; // increments the stack pointer to the next availible memory location
}

Value pop() {
    vm.stackTop--; // decrements the stack pointer
    return *vm.stackTop; // returns the dereferenced value (it can now be overwritten)
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++) //reads the value at the incremented instruction pointer
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()]) // looks up the next byte in bytecode and and looks up the value in the value table and returns it
#define BINARY_OP(op) do { double b = pop(); double a = pop(); push(a op b); } while(false) //  macro for executing a bianry op based on the input


    for (;;) {
#ifndef DEBUG_TRACE_EXECUTION
    print("            ");
    for(Value* slot = vm.stack; slot < vm.stackTop; slot++) { // prints the stack each time we execute an instruction
        printf("[");
        printValue(*slot);
        printf("]");
    }
    printf("\n");
    disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        // DECODING the instruction based on the opcode
        switch (instruction = READ_BYTE()) { // reads the byte currently being pointed to and figures out the type of instruction
            case OP_CONSTANT: {// pushes the constant onto the stack!!!
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD: BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE: BINARY_OP(/); break;
            case OP_NEGATE: push(-pop()); break; // pops the stack ansd pushes the negative value back on
            case OP_RETURN: // pops the stack and prints top value before exiting the progra
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk); // initializes a chunk

    if(!compile(source, &chunk)) { // throws an error if we get a compiler error
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk; // pass or chunk into the vm
    vm.ip = vm.chunk->code; // set the instruction pointer to the start of our code

    InterpretResult result = run(); // runs the result

    freeChunk(&chunk); // frees up the chunk
    return result; // returns the result
}