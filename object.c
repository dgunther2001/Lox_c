#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) { // allocates an object of the proper type
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    // allows us to insert objects onto our linked list of objects
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString* allocateString(char* chars, int length) { //  allocates an object WITH TYPE STRING (kind of like the "constructor")
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    return string;
}

ObjString* takeString(char* chars, int length) {
    return allocateString(chars, length);
}

ObjString* copyString(const char* chars, int length) {
    char* heapChars = ALLOCATE(char, length+1); // allocates a place on the heap up to the size of the string 
    memcpy(heapChars, chars, length); // copy over characters from the lexeme itself
    heapChars[length] = '\0'; // adds the string terminus
    return allocateString(heapChars, length); // calls the string allocation fucntion
}

void printObject(Value value) { // allows us to print out the native c values of a lox object
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}