#include <stdio.h>
#include <stdarg.h>

#include "debug.h"
#include "vm.h"
#include "compiler.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack; // just sets the stack pointer back to the base of the stack, which is just a pointer to an array anyways
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code -1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
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

static Value peekVM(int distance) {
    return vm.stackTop[-1 - distance]; // returns the value from the top of the stack, but doesn't push it
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++) //reads the value at the incremented instruction pointer
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()]) // looks up the next byte in bytecode and and looks up the value in the value table and returns it
#define BINARY_OP(valueType, op) do { if (!IS_NUMBER(peekVM(0)) || !IS_NUMBER(peekVM(1))) { runtimeError("Operands must be numbers."); return INTERPRET_RUNTIME_ERROR;} double b = AS_NUMBER(pop()); double a = AS_NUMBER(pop()); push(valueType(a op b)); } while(false) //  macro for executing a bianry op based on the input


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
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: BINARY_OP(NUMBER_VAL, +); break;
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                push(BOOL_VAL(isFalsey(pop())));
                break;
            case OP_NEGATE:  // pops the stack ansd pushes the negative value back on
                if (!IS_NUMBER(peekVM(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
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