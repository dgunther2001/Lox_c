#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <math.h>

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

    if (IS_LIST(args[0])) {
        ObjList* list = AS_LIST(args[0]); // takes the list in
        int len = listLength(list);  // calls the list length function
        Value valLen = NUMBER_VAL(len);
        return valLen;
    } else if (IS_STRING(args[0])) {
        ObjString* string = AS_STRING(args[0]);
        int len = string->length;
        Value valLen = NUMBER_VAL(len);
        return valLen;
    } else {
        return NUMBER_VAL(0);
    }
}


static Value scanNative(int argCount, Value* args) {

    char* str = (char*)malloc(1000); // allocates an array of size 20     
    scanf("%[^\n]%*c", str);

    int iterator = 0;
    char current = str[iterator];
    bool isNum = true;
    bool hasDot = false;
    while (current != '\0') {
        if (!(current >= '0' && current <= '9') && current != '.') {
            isNum = false;
        }
        if (current == '.' && hasDot == true) {
            isNum = false;
        }
        if (current == '.') {
            hasDot = true;
        }
        iterator++;
        current = str[iterator];
    }

    if (isNum == false) {
        ObjString* string;


        //string->length = strlen(str);
        string = takeString(str, strlen(str));

        Value returnVal  = OBJ_VAL(string);
        return returnVal;
    } else { // all
        double retVal =  atof(str);
        return NUMBER_VAL(retVal);
    }
    

}

static Value piNative(int argCount, Value* args){
    double pi = 3.141592653;
    return NUMBER_VAL(pi);
}

static Value eNative(int argcount, Value* args) {
    double e = 2.718281828;
    double exponent = AS_NUMBER(args[0]);
    double result = pow(e, exponent);
    return NUMBER_VAL(result);
}

static Value sqrtNative(int argcount, Value* args) {
    double rootVal = AS_NUMBER(args[0]);
    double result = sqrt(rootVal);
    return NUMBER_VAL(result);
}

static Value exponentiationNative(int argcount, Value* args) {
    double base = AS_NUMBER(args[0]);
    double exp = AS_NUMBER(args[1]);
    double result = pow(base, exp);
    return NUMBER_VAL(result);
}

static Value nthRootNative(int argcount, Value* args) {
    double value = AS_NUMBER(args[0]);
    double root = AS_NUMBER(args[1]);
    double result = pow(value, 1.0/root);
    return NUMBER_VAL(result);
}

static Value logNative(int argcount, Value* args) {
    double base = AS_NUMBER(args[0]);
    double value = AS_NUMBER(args[1]);
    double result = log2(value) / log2(base);
    return NUMBER_VAL(result);
}

static Value log10Native(int argcount, Value* args) {
    double value = AS_NUMBER(args[0]);
    double result = log10(value);
    return NUMBER_VAL(result);
}

static Value log2Native(int argcount, Value* args) {
    double value = AS_NUMBER(args[0]);
    double result = log2(value);
    return NUMBER_VAL(result);
}

static Value lnNative(int argcount, Value* args) {
    double base = 2.718281828;
    double value = AS_NUMBER(args[0]);
    double result = log2(value) / log2(base);
    return NUMBER_VAL(result);
}

static Value floorNative(int argcount, Value* args) {
    double val = AS_NUMBER(args[0]);
    double floor = floorf(val);
    return NUMBER_VAL(floor);
}

static Value ceilingNative(int argcount, Value* args) {
    double val = AS_NUMBER(args[0]);
    double ceiling = ceilf(val);
    return  NUMBER_VAL(ceiling);
}

static Value absvalNative(int argcount, Value* args) {
    double val = AS_NUMBER(args[0]);
    double absVal = fabs(val);
    return NUMBER_VAL(absVal);
}

static Value typeNative(int argcount, Value* args) {
    Value val = args[0];
    ObjString* type;

    if (IS_NUMBER(val)){
        type = copyString("num", 3);
    } else if (IS_BOOL(val)) {
        type = copyString("bool", 4);
    } else if (IS_NIL(val)) {
        type = copyString("nil", 3);
    } else if (IS_STRING(val)) {
        type = copyString("str", 3);
    } else if (IS_LIST(val)) {
        type = copyString("list", 4);
    } else if (IS_FUNCION(val)) {
        type = copyString("func", 4);
    } else if (IS_NATIVE(val)) {
        type = copyString("nvf", 3);
    } else if (IS_CLASS(val)) {
        type = copyString("class", 5);
    } else {
        type = copyString("other", 5);
    }

    return OBJ_VAL(type);
}

static Value toStringNative(int argcount, Value* args) {
    Value value = args[0];
    ObjString* retVal;
    char* str = (char*)malloc(1000);
    if (IS_NUMBER(value)) {
        sprintf(str, "%g", AS_NUMBER(value));
        retVal = takeString(str, strlen(str));
    } else if (IS_BOOL(value)) {
        if (value.as.boolean == true) {
            retVal = copyString("true", 4);
        } else {
            retVal = copyString("false", 5);
        }
    } else if (IS_NIL(value)) {
        retVal = copyString("nil", 3);
    } else if (IS_STRING(value)) {
        retVal = copyString(AS_STRING(value)->chars, AS_STRING(value)->length);
    } else {
        //runtimeError("Invalid type conversion.");
        retVal = copyString("", 0);
    }

    return OBJ_VAL(retVal);
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
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals); // initializes our hash table of global variables
    initTable(&vm.strings); // initializes our hash table of strings

    vm.initString = NULL; // deals with a potential garbage collector bug
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative);
    defineNative("len", lengthNative);
    defineNative("append", appendNative);
    defineNative("delete", deleteNative);
    defineNative("scan", scanNative);
    defineNative("pi", piNative);
    defineNative("exp", eNative);
    defineNative("sqrt", sqrtNative);
    defineNative("power", exponentiationNative);
    defineNative("nroot", nthRootNative);
    defineNative("log", logNative);
    defineNative("log10", log10Native);
    defineNative("log2", log2Native);
    defineNative("ln", lnNative);
    defineNative("floor", floorNative);
    defineNative("ceil", ceilingNative);
    defineNative("abs", absvalNative);
    defineNative("type", typeNative);
    defineNative("toString", toStringNative);
}

void freeVM() {
    freeTable(&vm.globals); // frees up the memory of the global variable hash table in the vm
    freeTable(&vm.strings); // frees up memory of the string hash table in the vm
    vm.initString = NULL;
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
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                vm.stackTop[-argCount - 1] = bound->receiver;
                return callVM(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                Value initializer;
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return callVM(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.", argCount);
                    return false;
                }
                return true;
            }
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

static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
    Value method;
    if(!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }
    return callVM(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, int argCount) {
    Value receiver = peekVM(argCount);

    if (!IS_INSTANCE(receiver)) { // if its not an instance (receiver), throw an error
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        vm.stackTop[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if(!tableGet(&klass->methods, name, &method)) { // look for the method in the classes method table
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peekVM(0), AS_CLOSURE(method)); // otherwise, wrap it up as a bound method

    pop();
    push(OBJ_VAL(bound)); // push the bound method onto the stack
    return true;
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

static void defineMethod(ObjString* name) {
    Value method = peekVM(0);
    ObjClass* klass = AS_CLASS(peekVM(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    // gets both initial strings off of the stack
    ObjString* b = AS_STRING(peekVM(0));
    ObjString* a = AS_STRING(peekVM(1));

    int length = a->length + b->length; // creates a cumulative length value
    char* chars = ALLOCATE(char, length+1); // allocates memory for the concatenated string
    memcpy(chars, a->chars, a->length); // copies a 
    memcpy(chars + a->length, b->chars, b->length); // copies b
    chars[length] = '\0'; // adds the terminus

    ObjString* result = takeString(chars, length); // gets the result
    pop();
    pop();
    push(OBJ_VAL(result)); // pushes the result in the form of an object onto the stack
}


static void concatenateNUM_STRING() {
    ObjString* b = AS_STRING(peekVM(0));
    double a_val = (AS_NUMBER(peekVM(1)));

    char* str = (char*)malloc(1000);
    sprintf(str, "%g", a_val);
    
    ObjString* a = takeString(str, strlen(str));
    
    int length = a->length + b->length; // creates a cumulative length value
    char* chars = ALLOCATE(char, length+1); // allocates memory for the concatenated string
    memcpy(chars, a->chars, a->length); // copies a 
    memcpy(chars + a->length, b->chars, b->length); // copies b
    chars[length] = '\0'; // adds the terminus

    ObjString* result = takeString(chars, length); // gets the result
    pop();
    pop();
    push(OBJ_VAL(result)); // pushes the result in the form of an object onto the stack
}

static void concatenateSTRING_NUM() {
    double b_val = (AS_NUMBER(peekVM(0)));
    char* str = (char*)malloc(1000);
    sprintf(str, "%g", b_val);
    ObjString* b = takeString(str, strlen(str));
    ObjString* a = AS_STRING(peekVM(1));

     int length = a->length + b->length; // creates a cumulative length value
    char* chars = ALLOCATE(char, length+1); // allocates memory for the concatenated string
    memcpy(chars, a->chars, a->length); // copies a 
    memcpy(chars + a->length, b->chars, b->length); // copies b
    chars[length] = '\0'; // adds the terminus

    ObjString* result = takeString(chars, length); // gets the result
    pop();
    pop();
    push(OBJ_VAL(result)); // pushes the result in the form of an object onto the stack
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
            /*
            case OP_DECLARE_PROPERTY: {

                tableSet(&class->fields, name, NULL);
            }
            */
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peekVM(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peekVM(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop();
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peekVM(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peekVM(1));
                tableSet(&instance->fields, READ_STRING(), peekVM(0));
                Value value = pop();
                pop();
                push(value);
                break;
            }
            case OP_GET_SUPER: {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop());

                if(!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                
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
                Value obj = pop();
                Value result;

                if (!IS_STRING(obj) && !IS_LIST(obj)) {
                    runtimeError("Invalid type to index to.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                if(!IS_NUMBER(index)) {
                    runtimeError("List index is not a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (IS_LIST(obj)) {
                    Value list = obj;

                    ObjList* listObj = AS_LIST(list);

                    int indexInt = AS_NUMBER(index);

                    if (!isValidListIndex(listObj, indexInt)) {
                        runtimeError("List index out of range.");
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    result = indexFromList(listObj, AS_NUMBER(index));
                    push(result);
                    break;
                } else if (IS_STRING(obj)) {
                    Value string = obj;

                    ObjString* stringObj = AS_STRING(string);

                    int indexInt = AS_NUMBER(index);

                    if (AS_NUMBER(index) > stringObj->length) {
                        runtimeError("String index out of range.");
                        return INTERPRET_RUNTIME_ERROR;
                    }

                    char resOut = stringObj->chars[indexInt];

                    char* out = (char*)malloc(sizeof(char) * 1);
                    out[0] = resOut;
                    ObjString* output = takeString(out, 1);
                    Value result = OBJ_VAL(output);

                    push(result);
                    break;
                }
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
            /*
            case OP_INCREMENT: {
                double num = AS_NUMBER(pop());
                double result = num++;
                push(NUMBER_VAL(result));
                break;
            }
            */
            case OP_ADD: {
                if (IS_STRING(peekVM(0)) && IS_STRING(peekVM(1))) {
                    concatenate();
                }  else if (IS_NUMBER(peekVM(1)) && IS_STRING(peekVM(0)))  {
                    concatenateNUM_STRING();
                } else if (IS_STRING(peekVM(1)) && IS_NUMBER(peekVM(0))) {
                    concatenateSTRING_NUM(); 
                } else if (IS_NUMBER(peekVM(0)) && IS_NUMBER(peekVM(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } /*else if (IS_NUMBER(peekVM(0)) && AS_STRING(peekVM(1))->chars =="+") {
                    double b = AS_NUMBER(pop());
                    pop();
                    b++;
                    push(NUMBER_VAL(b));
                }*/ else {
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
            case OP_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_SUPER_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop());
                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
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
            case OP_CLASS: 
                push(OBJ_VAL(newClass(READ_STRING()))); // take the string name from the constants table; create a new class with that name; push a pointer to the class object onto the stack
                break;
            case OP_INHERIT: {
                Value superclass = peekVM(1);
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass* subclass = AS_CLASS(peekVM(0));
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);  // essentilly full on method inheritance
                pop();
                break;
            }
            case OP_METHOD:
                defineMethod(READ_STRING());
                break;
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