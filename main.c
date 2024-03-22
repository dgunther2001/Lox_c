#include "common.h"
#include "chunk.h"
#include "chunk.c"
#include "debug.h"
#include "debug.c"
#include "memory.h"
#include "memory.c"
#include "value.h"
#include "value.c"

int main(int argc, const char* argv[]) {
    Chunk chunk; // declares a struct of type chunk
    initChunk(&chunk); // initializes a chunk

    int constant = addConstant(&chunk, 1.2); // adds a constant to the chunk value array and returns the index
    // two byte isntruction as we need both the opcode, and the parameters, in this case the index of the value
    writeChunk(&chunk, OP_CONSTANT, 123); // writes opcode of constant to the chunk
    writeChunk(&chunk, constant, 123); // writes the value of the index of the constant to the chunk

    writeChunk(&chunk, OP_RETURN, 123); // writes the return instruction to the chunk

    disassembleChunk(&chunk, "test chunk"); // takes machine code, and spits out actual human readable ASSEMBLY-LIKE mnemonics
    freeChunk(&chunk); // deallocates the chunk, but it remains initialized with a capacity of 0
    return 0;
}