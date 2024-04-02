#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
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
    PREC_SUBSCRIPT, // []
    //PREC_LAMBDA,
    PREC_PRIMARY
} Precendence;

typedef void (*ParseFn)(bool canAssign); // a void function that takes no arguments and returns nothing

typedef struct {
    ParseFn prefix; // function to compile prefix based on token of correct type
    ParseFn infix; // compile an infix expression with a left operand followed by token of expected type
    Precendence precedence; // the precedence level of the infix operation
} ParseRule;

typedef struct {
    Token name; // stores a token with a name
    int depth; // stores a depth (0: global, 1: 1st nest, ... )
    bool isCaptured; 
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    //TYPE_LAMBDA,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT,
} FunctionType; // creates an enum with our function type that is also strored within the compiler

typedef struct Compiler{
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT]; // scores an array of local variables with a size up to 8 bit unsigned integers
    int localCount; // how many local variables we are storing
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth; // an integer I imagine that we score a scope value we can compare based on how much nesting is going on
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperClass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL; // initialzes a Compiler pointer
ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() { // (if not in function call, just returns the chunk within the compiler)
    return &current->function->chunk; // returns the chunk in the current function instead
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

static bool check(TokenType type) {
    return parser.current.type == type; // checks to see if the parser contains the current type
}

// checks to matcg up tokens
static bool matchComp(TokenType type) {
    if (!check(type)) return false; // calls a helper check function
    advanceC();
    return true;
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

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP); // writes a byte with the loop operation to the chunk

    int offset = currentChunk()->count - loopStart + 2; // figures out the offset
    if (offset > UINT16_MAX) error("Loop body too large."); // if the offset to jump is too large, we throw an error

    emitByte((offset >> 8) & 0xff); // otherwise, we emit two bytes onto the stack indicationg the loop jump
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction); // emits a placeholder operand for the jump offset
    // allows for an up to 16 bit offset
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count -2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) { // if its an intializer, implicitly return the class
        emitBytes(OP_GET_LOCAL, 0); // the value 0 contains an instance (remember all of the receiver stuff)
    } else {
        emitByte(OP_NIL);
    }

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

static void patchJump(int offset) { // goes back in and patches the actual amount of offset we need to deal with at the if statement
    int jump = currentChunk()->count - offset - 2; // adjusts for the additional offset of the jump instruction itself as well as any additional offset

    if (jump > UINT16_MAX) {
        error("Tried to jump farther than 65,535 bytes.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) { // just intitialzes the compiler
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length); // if we're not in the basic "main" function, we add a name "metadata" ot the current compiler
    }

    // these next lines fundamentally reserve stack space allowing us to later refer to function calls
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function; 

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++; // increments the scope depth into a new scope
}

static void endScope() {
    current->scopeDepth--; // decrements the scope depth out of the current scope

    while (current->localCount > 0 && // iterates over an entire local scope
        current->locals[current->localCount -1].depth > current->scopeDepth
    ) {
        if (current->locals[current->localCount - 1].isCaptured) { // essentially, do we need to throw it onto the heap for later use??
            emitByte(OP_CLOSE_UPVALUE);
        } else{
            emitByte(OP_POP);
        }
        emitByte(OP_POP); // pops off the local variables
        current->localCount--; // decrements the number of local variables until we get to 0, thereby ending any trace of the scope
    } 
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precendence precedence);
static uint8_t identifierConstant(Token* name);
static int resolveLocal(Compiler* compiler, Token* name);
static void and_(bool canAssign);
static void or_(bool canAssign);
static uint8_t argumentList();
static int resolveUpvalue(Compiler* compiler, Token* name);

static void binary(bool canAssign) {
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

static void call(bool canAssign) {
    uint8_t argCount = argumentList(); // returns the number of arguments compiled
    emitBytes(OP_CALL, argCount); // chucks the calling operation, and the argument count onto the stack
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && matchComp(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name); // sets the property
    } else if (matchComp(TOKEN_LEFT_PAREN)) { // optimizes the invocation and method calls
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name); // OP_INVOKE is a combination of OP_GET_PORPERTY & OP_CALL => this is how you begin to optimize your compiler (PATTERNS!!!!!)
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name); // gets the property
    }
}

static void literal(bool canAssign) { // emits a byte of the literal into the chunk
    switch(parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        default: return;
    }
}

static void grouping(bool canAssign) {
    expression(); // deals with creating and evaluating the inner expression
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression."); // looks for a closing right parenthesis
}

/*
static void scan(bool canAssign) {
    emitByte(OP_SCAN);
}
*/

static void numberC(bool canAssign) { // this simply compiles number literals
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE); // if the left hand side is false, we go and evaluate the right
    int endJump = emitJump(OP_JUMP); // tiny jump over the next expression on other side of or if true

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void stringC(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void list(bool canAssign) {
    int itemCount = 0;
    if(!check(TOKEN_RIGHT_BRACKET)) {
        do {
            if (check(TOKEN_RIGHT_BRACKET)) {
                // trailing comma case
                break;
            }

            parsePrecedence(PREC_OR);

            if (itemCount == UINT8_COUNT) {
                error("Cannot have more than 256 items in a list literal");
            }
            itemCount++;
        } while (matchComp(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after list literal.");

    emitByte(OP_BUILD_LIST);
    emitByte(itemCount);
    return;
}

static void subscript(bool canAssign) {
    parsePrecedence(PREC_OR);
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after index.");

    if (canAssign && matchComp(TOKEN_EQUAL)) {
        expression();
        emitByte(OP_STORE_SUBSCR);
    } else {
        emitByte(OP_INDEX_SUBSCR);
    }
    return;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name); // look for the local variable
    if (arg != -1) { 
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    
    if (canAssign && matchComp(TOKEN_EQUAL)) { // verifies whether this is a valid assignment
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variableC(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if (!currentClass->hasSuperClass) {
        error("Can't use 'super' in class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    if (matchComp(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        namedVariable(syntheticToken("this"), false);
        namedVariable(syntheticToken("super"), false);
    }

    emitBytes(OP_GET_SUPER, name);
}

static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variableC(false);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type; // firgures out the previous token type

    parsePrecedence(PREC_UNARY); // compiles the expression

    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break; // emits a negationn byte and breaks
        //case TOKEN_PLUS_PLUS: emitByte(OP_INCREMENT); break;
        default: return;
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]      = {grouping, call, PREC_CALL},
    //[TOKEN_PLUS_PLUS]       = {unary, NULL, PREC_TERM},
    [TOKEN_RIGHT_PAREN]     = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]      = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACKET]    = {list, subscript, PREC_SUBSCRIPT},
    //[TOKEN_SCAN]            = {scan, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACKET]   = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]           = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT]             = {NULL, dot, PREC_CALL},
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
    [TOKEN_EXTENDS]         = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]      = {variableC, NULL, PREC_NONE},
    [TOKEN_STRING]          = {stringC, NULL, PREC_NONE},
    [TOKEN_NUMBER]          = {numberC, NULL, PREC_NONE},
    [TOKEN_AND]             = {NULL, and_, PREC_AND},
    [TOKEN_CLASS]           = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]            = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE]           = {literal, NULL, PREC_NONE},
    [TOKEN_FOR]             = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN]             = {NULL, NULL, PREC_NONE},
    //[TOKEN_LAMBDA]          = {NULL, NULL, PREC_NONE},
    [TOKEN_IF]              = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL]             = {literal, NULL, PREC_NONE},
    [TOKEN_OR]              = {NULL, or_, PREC_OR},
    [TOKEN_PRINT]           = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN]          = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER]           = {super_, NULL, PREC_NONE},
    [TOKEN_THIS]            = {this_, NULL, PREC_NONE},
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
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;  // essentially chekcs if the precedence of the left side is low enought to allow for assignment
    prefixRule(canAssign);
 
    while (precedence <= getRule(parser.current.type)->precedence) { 
        advanceC();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && matchComp(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length))); // adds a constant to the constant table, and returns the index it lives in within the values table
}

static bool identifiersEqual(Token* a, Token *b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0; // otherwise, it compares the actual identifier names letter by letter
}

static int resolveLocal(Compiler* compiler, Token* name) { 
    for (int i = compiler->localCount - 1; i >= 0; i--) { // walk the list of local variables (ensures the correct shadowing)
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) { // compares identifiers
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer."); // the default scope of an uninitialized variable is -1, so we throw an error here if it has only been declared
            }
            return i; // returns an index if the names are equal
        }
    }

    return -1; // otherwise returns -1 (NOT FOUND)
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount; // gets the current number of upvalues in a function

    for (int i = 0; i < upvalueCount; i++) { // if the upvalue already exists, we just print the index in which it lives
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure values in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal; // 
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true; // we set the local to captured
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // RECURSIVELY ADD UPVALUES TO THE ENCLOSING FUNCTIONS UNTIL IT GETS TO THE PROPER LOCATION
    int upvalue = resolveUpvalue(compiler->enclosing, name); // essentially works along the nested compilers until we find the value we're looking for
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name) { // allows us to create a local variable
    if (current->localCount == UINT8_COUNT) { // if we run out of space to store local variables, we throw an error
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++]; // initializes a new local variable
    local->name = name; // sets it to the name we declared for it
    local->depth = -1; // sets the variables depth accordingly
    local->isCaptured = false; // all variables are initially not captured
}

static void declareVariable() {
    if (current->scopeDepth == 0) return; // if its global, get out of the local declaration function

    Token* name = &parser.previous; // grab the variable name
    for (int i = current->localCount - 1; i >= 0; i--) { // iterates over the local current local variable array
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) { // the current depth is less the compiler scope depth, we break out and declare the variable
            break;
        }

        if (identifiersEqual(name, &local->name)) { // if the current identifier name in the array equals the identifier name we are trying to declare, we throw an error
            error("Already exists a variable with this name in the scope.");
        }
    }

    addLocal(*name); // generate a local, passing the dereferenced name
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage); // consumes the identidier

    declareVariable(); // we "declare" the variable
    if (current-> scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous); // returns what follows the identifier
}

static void markInitialized() {
    if (current->scopeDepth == 0) return; // returns for functions in global scope
    current->locals[current->localCount - 1].depth = current->scopeDepth; // sets the depth to the currents scope depth once the local variable has been initialized
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) { // cannot declare a global variable within scope
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if(!check(TOKEN_RIGHT_PAREN)) { // chew through the arguments so long as we see that they are followed by a comma and increment the argument count
        do {
            expression(); // evaluates the individual argument (what if its a function call...)
            if (argCount > 255) {
                error("Can't have greater than 255 arguments.");
            }
            argCount++;
        } while (matchComp(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE); // if the left operand is false, we discard and jump

    emitByte(OP_POP); // otherwise we pop the stack and evaluate the next expression
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type]; // returns the rule at the given index
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration(); // calls a declaration while we're within a block
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block."); // consumes a right brace once it has been found
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if(!check(TOKEN_RIGHT_PAREN)) { // if the next token is not a right parentheis
        do {
            current->function->arity++; // increments the number of parameters
            if (current->function->arity > 255) {
                errorAtCurrent("Cannot have more than 255 paramaters for a function.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (matchComp(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block(); // deals with the internals of the block (the actual function body itself)

    ObjFunction* function = endCompiler(); // ends the local compiler
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) { // iterates over passed upvalues and emits them as instructions (variable in szie based on the number of up values)
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) { // allows us to implicity return an object without writing "return this" in the object constructor
        type = TYPE_INITIALIZER;
    }

    function(type);
    emitBytes(OP_METHOD, constant);
}

/*
static void fieldDec() {

}
*/

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name."); // consumes the class name identifier
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous); // grabs the name
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant); // define the classes name as a variable

    ClassCompiler ClassCompiler;
    ClassCompiler.hasSuperClass = false;
    ClassCompiler.enclosing = currentClass;
    currentClass = &ClassCompiler;

    if(matchComp(TOKEN_EXTENDS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variableC(false);

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OP_INHERIT);
        ClassCompiler.hasSuperClass = true;
    }

    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    /*
    uint8_t fieldDecl;
    while(!check(TOKEN_RIGHT_BRACE)) {
        fieldDecl = identifierConstant(&parser.previous);
        emitBytes(OP_DECLARE_PROPERTY, fieldDecl);
        consume(TOKEN_SEMICOLON, "Expect semicolon after field declaration.");
    }
    */
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if (ClassCompiler.hasSuperClass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name."); // parses the function mae
    markInitialized(); // the function is initialized (cannot declare without speciying a body)
    function(TYPE_FUNCTION); // calls the function creation function
    defineVariable(global); // defines a global variable based on the function name
} 

static void varDeclaration() { // if we match a "var" token, we jump here
    uint8_t global = parseVariable("Expect variable name.");  // we parse the variable

    if (matchComp(TOKEN_EQUAL)){ // if we see ane quals sign, we parse the expression
        expression();
    } else {
        emitByte(OP_NIL); // otherwise, we do nothing
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration."); // consumes up until the semicolon

    defineVariable(global); // calls the define variable function
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'."); // consumes the leftmost parenthesis
    if(matchComp(TOKEN_SEMICOLON)) {
        // do nothing (no initialized value)
    } else if (matchComp(TOKEN_VAR)) {
        varDeclaration(); // calls a variable declaration
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count; 
    int exitJump = -1; // if this is non-negative 1, we presumably jump out of the loop
    if (!matchComp(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop exit condition.");

        // loop exit if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) { // some convoluted logic that jumps around and lets us increment
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for loop clause declaration.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement(); // evaluates the loop body
    emitLoop(loopStart); // jumps back if condition still valid

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }

    endScope();
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'."); // consumes the first parenthesis after the if keyword
    expression(); // evaluates the internal expression
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condtion."); // consumes up until the right parenthesis

    // CONDITION STORED AT THE TOP OF THE STACK

    int thenJump = emitJump(OP_JUMP_IF_FALSE); // jumps if false
    emitByte(OP_POP);
    statement(); 

    int elseJump = emitJump(OP_JUMP); // jumps if we have an else statement

    patchJump(thenJump);
    emitByte(OP_POP);

    if (matchComp(TOKEN_ELSE)) statement(); // if we match the token else, we call a statement(note that we've already jumped)
    patchJump(elseJump); 
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

/*
static void lambdaExpression() {
    consume(TOKEN_LEFT_PAREN, "Expect '('.");
    if(!check(TOKEN_RIGHT_PAREN)) { // if the next token is not a right parentheis
        do {
            
            current->function->arity++; // increments the number of parameters
            if (current->function->arity > 255) {
                errorAtCurrent("Cannot have more than 255 paramaters for a function.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (matchComp(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block(); // deals with the internals of the block (the actual function body itself)
}
*/

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }
    if (matchComp(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

/*
static void scanStatement() {
    expression();
    //statement();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_SCAN);
}
*/

static void whileStatement() {
    int loopStart = currentChunk()->count; // capture the location of compilation to jump back to
    consume(TOKEN_LEFT_PAREN, "Expect '(' before while loop condition."); // consumes the left parenthesis
    expression(); // evaluates the condition
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after while loop condition."); // consumes the right parenthesis

    int exitJump = emitJump(OP_JUMP_IF_FALSE); // initializes a jump if the condition is false
    emitByte(OP_POP); 
    statement(); // deals with the body of the function itself
    emitLoop(loopStart); // goes back to the start of the loop

    patchJump(exitJump);
    emitByte(OP_POP);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default: ;
        }
        advanceC();
    }
}

static void declaration() {
    if (matchComp(TOKEN_CLASS)) {
        classDeclaration();
    } else if (matchComp(TOKEN_FUN)) {
        funDeclaration();
    } else if (matchComp(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize(); // call the error function
}

static void statement() {
    if (matchComp(TOKEN_PRINT)) {
        printStatement();
    } /*else if (matchComp(TOKEN_SCAN)) {
        scanStatement();
    }*/ else if (matchComp(TOKEN_FOR)) {
        forStatement();
    } /*else if (matchComp(TOKEN_LAMBDA)) {
        lambdaExpression();
    }*/ else if (matchComp(TOKEN_IF)) {
        ifStatement();
    } else if (matchComp(TOKEN_RETURN)) {
        returnStatement();
    } else if (matchComp(TOKEN_WHILE)) {
        whileStatement();
    } else if (matchComp(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else { 
        expressionStatement();
    }
}

// THE ACTUAL COMPILER CALL
ObjFunction* compile(const char* source) {
    initScanner(source); // initalizes a scanner on the source
    Compiler compiler; // declares the compiler
    initCompiler(&compiler, TYPE_SCRIPT);  // initializes the compiler and sets the current compiler to this compiler
    
    // intialize our error handling
    parser.hadError = false;
    parser.panicMode = false;

    advanceC(); // primes the scanner
    
    while (!matchComp(TOKEN_EOF)) {
        declaration(); // keep on compiling declarations until we hit the end of file tag
    }

    ObjFunction* function = endCompiler(); // closes out of the compiler and returns the function
    return parser.hadError ? NULL : function; // if there was an error, return false, else return true
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}