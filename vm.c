#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"
#include "compiler.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value appendNative(int argCount, Value* args) {
    // append the value and increment length
    if (argCount != 2 || !IS_LIST(args[0])) {
        // error handling
    }
    ObjList* list = AS_LIST(args[0]);
    Value item = args[1];
    appendToList(list, item);
    return NIL_VAL;
}

static Value deleteNative(int argCount, Value* args) {
    if (argCount != 2  || !IS_LIST(args[0]) || !IS_NUMBER(args[1])) {
        // do some error handling
    }

    ObjList* list = AS_LIST(args[0]);
    int index = AS_NUMBER(args[1]);

    if (!isValidListIndex(list, index)) {
        // more error handling
    }

    deleteFromList(list, index);
    return NIL_VAL;
}

static Value lengthNative(int argCount, Value* args) {
    ObjList* list = AS_LIST(args[0]); // takes the list in
    int len = listLength(list);  // calls the list length function
    Value valLen = NUMBER_VAL(len);
    return valLen;
}


static Value scanNative(int argCount, Value* args) {

    char* str = (char*)malloc(1000); // allocates an array of size 20     
    scanf("%[^\n]%*c", str);
    ObjString* string;


    //string->length = strlen(str);
    string = takeString(str, strlen(str));

    Value returnVal  = OBJ_VAL(string);
    return returnVal;

}

static void resetStack() {
    vm.stackTop = vm.stack; // just sets the stack pointer back to the base of the stack, which is just a pointer to an array anyways
    vm.frameCount = 0; // sets the number of call frames back down to 0
    vm.openUpvalues = NULL; // list of upvalues in the VM begins empty
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() { // initializes the stack
    resetStack(); // just sets the stack pointer to 0
    vm.objects = NULL;

    initTable(&vm.globals); // initializes our hash table of global variables
    initTable(&vm.strings); // initializes our hash table of strings

    defineNative("clock", clockNative);
    defineNative("len", lengthNative);
    defineNative("append", appendNative);
    defineNative("delete", deleteNative);
    defineNative("scan", scanNative);

}

void freeVM() {
    freeTable(&vm.globals); // frees up the memory of the global variable hash table in the vm
    freeTable(&vm.strings); // frees up memory of the string hash table in the vm
    freeObjects(); // frees up the linked list of objects we were storing
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

static bool callVM(ObjClosure* closure, int argCount) { // essentialy initializes the call frame onto the stack
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments, but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if(IS_OBJ(callee)) { // if the callee is an object type
        switch(OBJ_TYPE(callee)) {
            case OBJ_CLOSURE:
                return callVM(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value result  = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break;
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    /* REASONS TO EXIT THE FOLLOWING LOOP
    *  1. the local slot we stopped at is what we're looking for
    *  2. we ran out of upvalues to search, so its just NULL
    *  3. we found an upvalue whose slot is lower on the stack than what we're looking for, so the one we're looking for must not be there
    */
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local); // creates a new upvalue that captures the stack slot and returns it
    createdUpvalue->next = upvalue; //sets us up to insert the upvalue into the list, so that it points the the next upvalue

    if(prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue; // initializes the list, if this is the first upvalue
    } else {
        prevUpvalue->next = createdUpvalue; // inserts the new upvalue into the list
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) { // takes a pointer to a stack slot
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    // gets both initial strings off of the stack
    ObjString* b = AS_STRING(pop()); 
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length; // creates a cumulative length value
    char* chars = ALLOCATE(char, length+1); // allocates memory for the concatenated string
    memcpy(chars, a->chars, a->length); // copies a 
    memcpy(chars + a->length, b->chars, b->length); // copies b
    chars[length] = '\0'; // adds the terminus

    ObjString* result = takeString(chars, length); // gets the result
    push(OBJ_VAL(result)); // pushes the result in the form of an object onto the stack
}

/*
static void concatenateNUM_STRING() {
    ObjString* b = AS_STRING(pop());
    Value a_val = NUMBER_VAL(AS_NUMBER(pop()));
    
    ObjString* a = AS_STRING(a_val);

    int length = a->length + b->length; // creates a cumulative length value
    char* chars = ALLOCATE(char, length+1); // allocates memory for the concatenated string
    memcpy(chars, a->chars, a->length); // copies a 
    memcpy(chars + a->length, b->chars, b->length); // copies b
    chars[length] = '\0'; // adds the terminus

    ObjString* result = takeString(chars, length); // gets the result
    push(OBJ_VAL(result)); // pushes the result in the form of an object onto the stack
    
    
}
*/

static void concatenateSTRING_NUM() {

}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1]; // stores the topmost callframe in a local variable
#define READ_BYTE() (*frame->ip++) //reads the value at the incremented instruction pointer
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()]) // looks up the next byte in bytecode and and looks up the value in the value table and returns it
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1])) // takes the next two bytes in the chunk and creates a two bit integer out of them
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) do { if (!IS_NUMBER(peekVM(0)) || !IS_NUMBER(peekVM(1))) { runtimeError("Operands must be numbers."); return INTERPRET_RUNTIME_ERROR;} double b = AS_NUMBER(pop()); double a = AS_NUMBER(pop()); push(valueType(a op b)); } while(false) //  macro for executing a bianry op based on the input


    for (;;) {
#ifndef DEBUG_TRACE_EXECUTION
    printf("            ");
    for(Value* slot = vm.stack; slot < vm.stackTop; slot++) { // prints the stack each time we execute an instruction
        printf("[");
        printValue(*slot);
        printf("]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code)); //instead reading from the current call frame
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
            case OP_POP: pop(); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE(); // finds where the local lives
                push(frame->slots[slot]); // pushes it onto the stack?
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peekVM(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING(); // reads the string
                Value value; // declares a value variable (our generic object)
                if(!tableGet(&vm.globals, name, &value)) { // causes a runtime error if we can't retieve the value because it DNE, but the tableGet(...) call populates the value memory location 
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value); // pushes what is now in value, so it exists on the stack for us to read
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING(); 
                tableSet(&vm.globals, name, peekVM(0)); 
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peekVM(0))) { // throws a runtime error if the variable we're trying to reset deosn't exist in the global variable hash table
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peekVM(0);
                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_BUILD_LIST: {
                ObjList* list = newList();
                uint8_t itemCount = READ_BYTE();

                push(OBJ_VAL(list));
                for (int i = itemCount; i > 0; i--) {
                    appendToList(list, peekVM(i));
                }
                pop();

                while (itemCount-- > 0) {
                    pop();
                }

                push(OBJ_VAL(list));
                break;
            }
            case OP_INDEX_SUBSCR: {
                Value index = pop();
                Value list = pop();
                Value result;

                if (!IS_LIST(list)) {
                    runtimeError("Invalid type to index to.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* listObj = AS_LIST(list);

                if(!IS_NUMBER(index)) {
                    runtimeError("List index is not a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int indexInt = AS_NUMBER(index);

                if (!isValidListIndex(listObj, indexInt)) {
                    runtimeError("List index out of range.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                result = indexFromList(listObj, AS_NUMBER(index));
                push(result);
                break;
            }
            case OP_STORE_SUBSCR: {
                Value item = pop();
                Value index = pop();
                Value list = pop();

                if (!IS_LIST(list)) {
                    runtimeError("Cannot store value in a non-list.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjList* listObj = AS_LIST(list);

                if (!IS_NUMBER(index)) {
                    runtimeError("List index is not a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                int indexInt = AS_NUMBER(index);

                if(!isValidListIndex(listObj, indexInt)) {
                    runtimeError("Invalid list index.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                storeToList(listObj, indexInt, item);
                push(item);
                break;
            }
            case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peekVM(0)) && IS_STRING(peekVM(1))) {
                    concatenate();
                } /* else if (IS_NUMBER(peekVM(0)) && IS_STRING(peekVM(1)))  {
                    concatenateNUM_STRING();
                } else if (IS_STRING(peekVM(0)) && IS_NUMBER(peekVM(1))) {
                    concatenateSTRING_NUM(); 
                } */ else if (IS_NUMBER(peekVM(0)) && IS_NUMBER(peekVM(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError(
                        "Operands must be two numbers or two strings."
                    );
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            
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
            case OP_PRINT: {
                printValue(pop()); // pops the resultant value that has already been pushed to the top of the stack
                printf("\n");
                break;
            }
            /*
            case OP_SCAN: {
                // PUSH THE VALUE ONTO THE STACK
                
                char* str = (char*)malloc(sizeof(char) * 1000); // allocates an array of size 20
            
                ObjString* string;
                scanf("%s", str);
                //printf("%s", str);
                
                string->chars = str;
                //printf("%s", string->chars);
                string->length = strlen(string->chars);
                //printf("%d", string->length);
            
                ObjString* result = takeString(string->chars, string->length);
                push(OBJ_VAL(result));
                //FREE(ObjString*, string);

                break;
            }
            */
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT(); // reads 2 bits instead
                if (isFalsey(peekVM(0))) frame->ip += offset; // if the condition is false, we jump the instruction pointer...
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT(); // gets the offset within the loop
                frame->ip -= offset; // jumps back to the start of the loop if called
                break;
            }
            
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peekVM(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame =&vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT()); // gets the function on the stack
                ObjClosure* closure = newClosure(function); // initializes the closure
                push(OBJ_VAL(closure)); // pushes the closure onto the stack
                for (int i = 0; i < closure->upvalueCount; i++) { // works semui-recursively so that we jump to the further enclosing function to capture the upvalue there if its not local
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop -1); // moves the upvalue from the stack to the heap to be stored for later
                pop();
                break;
            case OP_RETURN: {
                Value result = pop(); // gets the result
                closeUpvalues(frame->slots);
                vm.frameCount--; // decrements the frame count
                if (vm.frameCount == 0) { // if its the EOF, it interpreted OK
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots; // reset the stack pointer back to the frame pointer
                push(result); // push the return value
                frame = &vm.frames[vm.frameCount - 1]; // go back a frame
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    callVM(closure, 0);

    return run();
}