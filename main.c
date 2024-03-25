#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "chunk.c"
#include "debug.h"
#include "debug.c"
#include "memory.h"
#include "memory.c"
#include "value.h"
#include "value.c"
#include "vm.h"
#include "vm.c"
#include "scanner.h"
#include "scanner.c"
#include "compiler.h"
#include "compiler.c"
#include "object.h"
#include "object.c"
#include "table.h"
#include "table.c"

static void repl() { // interpretes our code line by measly line
    char line[1024]; // creatse a line array of 1024 characters
    for(;;) {
        printf(">>> "); // prints out a character to make us feel like we're in the terminal

        if(!fgets(line, sizeof(line), stdin)) { // if we enter (I think)
            printf("\n");
            break;
        }

        interpret(line);
    }
}

static char* readFile(const char* path) { // creates a massive character buffer that holds the entire contents of the file ina dynamically allocated array
    FILE* file = fopen(path, "rb");
    if (file == NULL) { // if the file isn't readable, or doesn;t exist, we "throw" an error
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END); // seek the end
    size_t fileSize = ftell(file); // this tells us how many bytes we are from the start of the file
    rewind(file); // goes back to the beginnijng of the file

    char* buffer = (char*)malloc(fileSize + 1); // allocates a buffer based on the size of the file
    if (buffer == NULL) { // throws an error if we can't allocate the buffer isteld because there isn't enough memory
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file); 
    if (bytesRead < fileSize) { // throws an error and exits if the file read fails as well
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) { // runs and executes abn entire file
    char* source = readFile(path);
    InterpretResult result = interpret(source);
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char* argv[]) {
    initVM(); // generates a virtual machine before actually doing anything

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: clos [path]\n");
        exit(64);
    }

    freeVM(); // frees up the memory space allocted to the vm
    return 0;
}