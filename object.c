#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
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

static ObjString* allocateString(char* chars, int length, uint32_t hash) { //  allocates an object WITH TYPE STRING (kind of like the "constructor")
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    tableSet(&vm.strings, string, NIL_VAL); // all the values are just nil
    return string;
}

static uint32_t hashString(const char* key, int length) { // FNV-1a HASH ALGORITHM
    uint32_t hash = 2166136261u; // start with some specific hash value
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(chars, chars, length + 1); // frees up memory for the string that was passed in
        return interned; // return the memory address of the string in the hash table
    }
    return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length); // gets the hash value of the string
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned; // just return a reference if the string already exists instead of creating a new one
    char* heapChars = ALLOCATE(char, length+1); // allocates a place on the heap up to the size of the string 
    memcpy(heapChars, chars, length); // copy over characters from the lexeme itself
    heapChars[length] = '\0'; // adds the string terminus
    return allocateString(heapChars, length, hash); // calls the string allocation fucntion
}

void printObject(Value value) { // allows us to print out the native c values of a lox object
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}