#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2;

/*
PARAMS ->       oldSize         newSize         Operation       
                   0            Non-zero        Allocate a new block
                Non-zero           0            Deallocation?
                Non-zero        < oldSize       Shrink block of allocated memory
                No-zero         > oldSize       Grow block of allocated memory
*/
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC // perpetually in garbage collect mode if the is defined
    collectGarbage();
#endif

    if (vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }
    }

    if (newSize == 0) { // frees up the pointer and returns null if we are deallocatin
        free(pointer);
        return NULL;
    }

    // realloc is equivalent to malloc when newSize = 0, so this is the catchall for the other three cases not accounted for
    void* result = realloc(pointer, newSize); // reallocates a new pointer with a piece of reallocated memory that uses the initial pointer, but allocates the correct size
    if (result == NULL) exit(1); // if there isn't enough memory for realloc, we exit the program
    return result;
}

void markObject(Obj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = (Obj**)realloc(vm.grayStack, (sizeof(Obj*) * vm.grayCapacity));
        if (vm.grayStack == NULL) exit(1);
    }
    vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
    if (IS_OBJ(value)) markObject(AS_OBJ(value)); // fist we verify that this is an actual heap object we need to deal with
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type) {
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_UPVALUE: 
            markValue(((ObjUpvalue*)object)->closed);
            break;
        case OBJ_LIST: {
            ObjList* list = (ObjList*)object;
            for (int i = 0; i < list->count; i++) {
                markValue(list->items[i]);
            }
            break;
        }
        case OBJ_NATIVE:
            break;
        case OBJ_STRING:
            break;
    }
}


static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch(object->type) {
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object; // casts the object to a function type
            freeChunk(&function->chunk); // frees up the chunk
            FREE(ObjFunction, object); // frees up the allocated space to the function itself
            break; // breaks
        }
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }
        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object; // sets a pointer to the passed object pointer
            FREE_ARRAY(char, string->chars, string->length + 1); // calls the free array function, so we no longer store pertinent information
            FREE(ObjString, object); // frees up the object itself
            break;
        }
        case OBJ_LIST: {
            ObjList* list = (ObjList*)object;
            FREE_ARRAY(Value*, list->items, list->count);
            FREE(ObjList, object);
            break;
        }
    }
}

static void markRoots() {
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) { // walks the roots within the range of the stack
        markValue(*slot); // marks the value at the dereferenced slott (WILL ADD ANOTHER FIELD MOST LIKELY)
    }

    for (int i = 0; i < vm.frameCount; i++) { // deals with the callframe pointers
        markObject((Obj*)vm.frames[i].closure);
    }

    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals); // deals with marking global variables
    markCompilerRoots();
}

static void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;
    
    while (object != NULL) { // walks every simgle object in the heap
        
        if (object->isMarked) { // if its black, we leave it alone
            object->isMarked = false;
            previous = object;
            object = object->next;
            
        } else { // if it's white, we unlink it, and free up the space
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }
            freeObject(unreached);
            
        }
        
        
    }
    
    
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

   markRoots();
   traceReferences();
   tableRemoveWhite(&vm.strings);
   sweep();

   vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("    collected %zu bytes (from %zu to %zu) next at %zu\n", before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif

}

void freeObjects() {
    Obj* object = vm.objects; // gets the head of the linked list of objects
    while (object != NULL) {
        Obj* next = object->next; // saves a pointer to the next object
        freeObject(object); // frees up the current object
        object = next; // goes to where the next pointer points to
    }
    free(vm.grayStack);
}
