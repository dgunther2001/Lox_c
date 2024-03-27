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

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION); // allocates a function object
    function->arity = 0; // sets the number of input parameters to 0
    function->name = NULL; // sets the name of the function to NULL
    initChunk(&function->chunk); // initializes a chunk within the function object to do all of the function operations
    return function; // returns the initialized function object
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

static void printFunction(ObjFunction* function) {
    if (function->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars); //prints out the function name
}

ObjList* newList() {
    ObjList* list = ALLOCATE_OBJ(ObjList, OBJ_LIST); // allocates an object of type list
    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
    return list;
}

void appendToList(ObjList* list, Value value) { // append to the end of a list
    if (list->capacity < list->count + 1) { // grows the list capacity dynamically if needed
        int oldCapacity = list->capacity;
        list->capacity = GROW_CAPACITY(oldCapacity);
        list->items = GROW_ARRAY(Value, list->items, oldCapacity, list->capacity);
    }
    list->items[list->count] = value; //adds the desired value to the end of the list
    list->count++; // increments the count
    return;
}

int listLength(ObjList* list) { // returns the length of the list
    return list->count;
}

void storeToList(ObjList* list, int index, Value value) { // change the value at a particular index
    list->items[index] = value;
}

Value indexFromList(ObjList* list, int index) { // returns the value of a list a specific index
    return list->items[index];
}

void deleteFromList(ObjList* list, int index) {
    for (int i = index; i < list->count - 1; i++) { // moves everything after deletion index and copies it one back
        list->items[i] = list->items[i+1];
    }
    list->items[list->count - 1] = NIL_VAL; // sets the end to a null value
    list->count--;
}

bool isValidListIndex(ObjList* list, int index) {
    if (index < 0 || index > list->count - 1) {
        return false;
    }
    return true;
}


static void printList(ObjList* list) {
    printf("[");
    for (int i = 0; i < list->count; i++) {
        Value currentItem = list->items[i];
        ValueType currentType = currentItem.type;
        switch(currentType) {
            case VAL_BOOL: {
                int boolInt = (currentItem).as.boolean;
                if (boolInt == 1) {
                    printf("%s", "true");
                } else {
                    printf("%s", "false");
                }
                break;
            }
            case VAL_NIL:
                printf("nil");
                break;
            case VAL_NUMBER:
                printf("%g", AS_NUMBER(currentItem));
                break;
            case VAL_OBJ: {
                printObject(currentItem);
                break;
            }
        }

        if (i != list->count - 1) {
            printf(", ");
        }
        
    }
    printf("]");
}



void printObject(Value value) { // allows us to print out the native c values of a lox object
    switch(OBJ_TYPE(value)) {
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_LIST:
            printList(AS_LIST(value));
            break;
    }
}
