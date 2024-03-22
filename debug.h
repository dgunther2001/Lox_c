#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name); // disassembles an entire chunk of code
int disassembleInstruction(Chunk* chunk, int offset); // disassembles a single instruction

#endif