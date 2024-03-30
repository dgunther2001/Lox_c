#ifndef clox_common_h
#define clox_common_h

/*
* Common and useful libraries throughout the entire project
*/
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

//#define NAN_BOXING
//#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION

//#define DEBUG_STRESS_GC // when this flag is defined, the garbage collector runs as often as feasibly possible (HORRIFIC IN ACTUALITY, BUT GREAT FOR DEBUGGING)
//#define DEBUG_LOG_GC // prints stuff to us when we do something with memory

#define UINT8_COUNT (UINT8_MAX + 1)

#endif