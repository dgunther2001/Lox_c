#include <stdio.h>

#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);  
    for(int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset); // disassembles each instruction in a chunk of instructions
    }
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1]; // gets the actual constant value
    printf("  %-16s %4d '", name, constant);  // prints out the instruction name and the associated constant location
    printValue(chunk->constants.values[constant]); // pringts out the actual constant value
    printf("'\n");
    return offset + 2; // iterates the offest by 2 because this is a 2 byte instruction
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name); // prints out the name of the instruction
    return offset + 1; // iterates the offset when we return back to the dissasembleChunk(...) method and returns
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset); // prints out the byte offset of the instruction (line number for now)
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) { // if the value in lines (storing literal line integers) are equal, then we print a pipe 
        printf("   |  ");
    } else { 
        printf("%4d", chunk->lines[offset]); // otherwise, we just print the line number as a 4 bit number
    }

    uint8_t instruction = chunk->code[offset]; // gets the instruction based on the offset
    switch(instruction) { // debugs for each possible instruction
        case OP_RETURN: 
            return simpleInstruction("OP_RETURN", offset);
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_NIL:
            return simpleInstruction("OP_NIL", offset);
        case OP_TRUE:
            return simpleInstruction("OP_TRUE", offset);
        case OP_FALSE:
            return simpleInstruction("OP_FALSE", offset);
        case OP_EQUAL:
            return simpleInstruction("OP_EQUAL", offset);
        case OP_GREATER:
            return simpleInstruction("OP_GREATER", offset);
        case OP_LESS:
            return simpleInstruction("OP_LESS", offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NOT:
            return simpleInstruction("OP_NOT", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        default:
            printf("%s %d\n", "Unknown opcode", instruction);
            return offset+1; // iterates the offset when we return back to the dissasembleChunk(...) method and returns
    }
}