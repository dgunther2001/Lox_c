#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifndef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct { // simply stores the current and previous token, so its "view" is very limited
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum { // precedence levels in order from lowest to highest
    PREC_NONE, // no precedence
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == 1=
    PREC_COMPARISON, // < <= > >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // - !
    PREC_CALL, // . ()
    PREC_PRIMARY
} Precendence;

typedef void (*ParseFn)(); // a void function that takes no arguments and returns nothing

typedef struct {
    ParseFn prefix; // function to compile prefix based on token of correct type
    ParseFn infix; // compile an infix expression with a left operand followed by token of exprected type
    Precendence precedence; // the precedence level of the infix operation
} ParseRule;

Parser parser;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk; // intermediary between the current emitted byte and the compiler (passes the current chunk being compiled)
}


/*
* BASIC TOKEN CONSUMPTION AND PARSING => front end
*/
static void errorAt(Token* token, const char* message) {
    if (parser.panicMode == true) return; // no more error cascading and craziness once we enter panic mode
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line); // prints where the error occured

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // do nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start); // prints out where the token is
    }

    fprintf(stderr,": %s\n", message); // prints out the error message
    parser.hadError = true; // sets the error flag to true
}

static void error(const char* message) { // reporting an error at the previously consumed token
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) { // throws an error if the scanner passes us an error token
    errorAt(&parser.current, message);
}

static void advanceC() {
    parser.previous = parser.current; // advances the parser

    for(;;) {
        parser.current = scanToken(); // scans a token
        if(parser.current.type != TOKEN_ERROR) break; // if its not an error, we break out of the loop

        errorAtCurrent(parser.current.start); // throws an error
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) { // verifies whether the current token is of the expected type, and then it is consumerd
        advanceC();
        return;
    }

    errorAtCurrent(message); // otherwise we throw an error
}

/*
* TRANSLATING INTO BYTECODE FOR OUR VM => back end
*/
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line); // writes the given byte we care about and sends the previous lines token for error handling purposes (runtime errors)
}

static void emitBytes(uint8_t byte1, uint8_t byte2) { // when we have two byte chunks (value storing, etc...)
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) { // generates a constant IF we aren;t storing too many
    int constant = addConstant(currentChunk(), value); // adds a constant
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk."); // some basic error handling
        return 0;
    }

    return (uint8_t)constant; // returns a the typecasted to 8 bit unsigned int: constant
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void endCompiler() {
    emitReturn();
#ifndef DEBIG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precendence precedence);

static void binary() {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precendence)(rule->precedence + 1));
    switch (operatorType) {
        case TOKEN_BANG_EQUAL:      emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:     emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:         emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL:   emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:            emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:      emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:            emitByte(OP_ADD); break;
        case TOKEN_MINUS:           emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:            emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:           emitByte(OP_DIVIDE); break;
        default:
            return;
    }
}

static void literal() { // emits a byte of the literal into the chunk
    switch(parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        default: return;
    }
}

static void grouping() {
    expression(); // deals with creating and evaluating the inner expression
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression."); // looks for a closing right parenthesis
}

static void numberC() { // this simply compiles number literals
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void unary() {
    TokenType operatorType = parser.previous.type; // firgures out the previous token type

    parsePrecedence(PREC_UNARY); // compiles the expression

    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break; // emits a negationn byte and breaks
        default: return;
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]      = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN]     = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]      = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]           = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT]             = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS]           = {unary, binary, PREC_TERM},
    [TOKEN_PLUS]            = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON]       = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH]           = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR]            = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG]            = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL]      = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL]           = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL]     = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER]         = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL]   = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS]            = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]      = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]      = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING]          = {NULL, NULL, PREC_NONE},
    [TOKEN_NUMBER]          = {numberC, NULL, PREC_NONE},
    [TOKEN_AND]             = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS]           = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]            = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE]           = {literal, NULL, PREC_NONE},
    [TOKEN_FOR]             = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN]             = {NULL, NULL, PREC_NONE},
    [TOKEN_IF]              = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL]             = {literal, NULL, PREC_NONE},
    [TOKEN_OR]              = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT]           = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN]          = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER]           = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS]            = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE]            = {literal, NULL, PREC_NONE},
    [TOKEN_VAR]             = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE]           = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF]             = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precendence precedence) {
    advanceC(); // read the next token
    ParseFn prefixRule = getRule(parser.previous.type)->prefix; // look up the corresponding parse rule
    if (prefixRule == NULL) {
        error("Exprect expression.");
        return;
    }

    prefixRule(); // otherwise call the prefix parse function 
 
    while (precedence <= getRule(parser.current.type)->precedence) { 
        advanceC();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type]; // returns the rule at the given index
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

// THE ACTUAL COMPILER CALL
bool compile(const char* source, Chunk* chunk) {
    initScanner(source); // initalizes a scanner on the source 
    compilingChunk = chunk; // sets the current chunk being compiled to the chunk passed to the compiler

    // intialize our error handling
    parser.hadError = false;
    parser.panicMode = false;

    advanceC(); // primes the scanner
    expression(); // evaluates a single expression
    consume(TOKEN_EOF, "Expect end of expression."); // look for the end of file token
    endCompiler(); // closes out of the compiler
    return !parser.hadError; // if there was an error, return false, else return true
}