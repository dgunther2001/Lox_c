#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct { // initialzes a scanner object that goes character by character, and also traces the current line number;
    const char* start;
    const char* current;
    int line;
} Scanner;

Scanner scanner; // initializes a scanner struct

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';  // checks whether a character is a valid alphabetic character (ASCII)
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9'; // if the character is a valid number, we return true, otherwise, we return false (DEALING IN ASCII VALUES)
}

static bool isAtEnd() {
    return *scanner.current == '\0'; // checks to see if the dereferenced scanner is at the terminus of the file (just a long string...)
}

static char advance() { // consumes and returns the current character
    scanner.current++;
    return scanner.current[-1];
}

static char peek() { // allows us to see the next character in the sequence being scanned
    return *scanner.current; // returns the value at the current character pointer in the scanner (already passed what we are currently evaluating because of advance)
}

static char peekNext() { // peeks a character ahead, but DOES NOT CONSUME THE FIRST CHARACTER
    if(isAtEnd()) return '\0';
    return scanner.current[1];
}

static bool match(char expected) {
    if (isAtEnd()) return false; // if we're at the file end, we return false
    if (*scanner.current != expected) return false; // if the current character (remember we iterated forward with the advance function) doesn't match the expected, we return false
    scanner.current++; // otherwise, we consume the matched charcter and ->
    return true; // return true
}

static Token makeToken(TokenType type) { // creates a token
    Token token; // initializes a token
    token.type = type; // sets the type to the passed type
    token.start = scanner.start; // sets the token start to where the scanner start is currently
    token.length = (int)(scanner.current - scanner.start); // sets the token length to the difference between the scanner current and start pointers
    token.line = scanner.line; // sets the line to the current line of the scanner
    return token; // returns the now fully defined token
}

static Token errorToken(const char* message) { // very similar to the makeToken function, but just returns an error type to the compiler with a pointer to an error message
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = scanner.line;
    return token;
}

static void skipWhitespace() {
    for(;;) { // iterates until we no longer have whitespace in the next character
        char c = peek(); // peeks the next character
        switch (c) { 
            case ' ':
            case '\r':
            case '\t':
                advance(); // advances and loops again if it's whitespace
                break;
            case '\n':
                scanner.line++; // iterates the line counter and advances if it's a newline character
                advance();
                break;
            case '/':
                if(peekNext() == '/') { // if we see another forward slach, we consume to the end of the line
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
                    return;
                } // doesn't do anthing and does a divide if there isn't a second '/'
                break; // it breaks if it was a comment after the loop is done and we reach the next line or the end of the file
            default:
                return; // returns back to our scanner with the current pointer updated once there is no more whitespace
        }
    }
}

static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
    if (scanner.current - scanner.start == start + length && memcmp(scanner.start + start, rest, length) == 0) { 
        /* 
        * if the lengths of the keyword and the potentially identifer do not match, we just return a TOKEN_IDENTIFIER to the compiler
        * otherwise we do a memcmp to do a lexical comparison with the rest fo the keyword, if they are equal, the memcmp call will return 0
        * this will thus allow us to instead return the expected token, which is not an identifier
        */
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static TokenType identifierType() { // USING A SMALL DFA TO CHECK IF THE IDENTIFIER MATCHES A KEYWORD
    switch (scanner.start[0]) { // big switch statement to see if the first character matches any of the keywords' first characters
        case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND); // sees if we match AND
        case 'c': return checkKeyword(1, 4, "lass", TOKEN_CLASS); // CLASS 
        case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE); // ELSE
        case 'f': // deals with our greater amalgamation of keywords that start with the letter f
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE); // FALSE
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR); // FOR
                    case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN); // FUN
                }
            }
            break;
        case 'i': return checkKeyword(1, 1, "f", TOKEN_IF); // IF
        case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL); // NIL
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR); // OR
        case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT); // PRINT
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN); // RETURN
        case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER); // SUPER
        case 't':
            if (scanner.current - scanner.start > 1) {
                switch(scanner.start[1]) {
                    case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS); // THIS
                    case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE); // TRUE
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR); // VAR
        case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE); // WHILE
    }
    return TOKEN_IDENTIFIER; // returns an identifier token to the compiler
}

static Token identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance(); // if the next character is an alphanumeric, we consume it and advance, as it is part of the identifier
    return makeToken(identifierType()); // returns an identifier token to the compiler, but it is type dependent, so we call another function
}

static Token number() {
    while (isDigit(peek())) advance(); // while the next character is a digit, we consume and advance

    if (peek() == '.' && isDigit(peekNext())) { // if we have a decimal point, and the following character is another digit, we consume them
        advance();

        while(isDigit(peek())) advance(); // then we consume the rest of the numbers after the decimal point
    }

    return makeToken(TOKEN_NUMBER); // creats a number token for the compiler
}

static Token string() {
    while(peek() != '"' && !isAtEnd()) { // while we're not at the end of the file, or at the string terminus (another doublequote)
        if (peek() == '\n') scanner.line++; // support for multiline strings
        advance(); // consumes the character
    }

    if(isAtEnd()) return errorToken("Undetermined string."); // if we get the the end of the file without closing the string, we pass an error token to the compiler

    advance(); // consumes the "
    return makeToken(TOKEN_STRING); // returns a token of type string
}

Token scanToken() {
    skipWhitespace(); // skips whitespace and advances the current pointer as such consuming the whitespace
    scanner.start = scanner.current; // at the beginning of a token when we call the function, so this is useful and valud

    if(isAtEnd()) return makeToken(TOKEN_EOF); // if we're at the end of the file, we pass an EOF token to the compiler

    char c = advance(); // advances the character and returns the character

    if(isAlpha(c)) return identifier(); // if the character is alphabetic, we call the identifier function, which passes an identifier token to the compiler
    if(isDigit(c)) return number(); // if the character is a number, we call the number function, which creates a number token that gets passed to the compiler

    switch(c) { 
        // makes a token based on the returned character from the advance function if it matches one of our single character lexemes
        case '(': return makeToken(TOKEN_LEFT_PAREN);
        case ')': return makeToken(TOKEN_RIGHT_PAREN);
        case '[': return makeToken(TOKEN_LEFT_BRACKET);
        case ']': return makeToken(TOKEN_RIGHT_BRACKET);
        case '{': return makeToken(TOKEN_LEFT_BRACE);
        case '}': return makeToken(TOKEN_RIGHT_BRACE);
        case ';': return makeToken(TOKEN_SEMICOLON);
        case ',': return makeToken(TOKEN_COMMA);
        case '.': return makeToken(TOKEN_DOT);
        case '-': return makeToken(TOKEN_MINUS);
        case '+': return makeToken(TOKEN_PLUS);
        case '/': return makeToken(TOKEN_SLASH);
        case '*': return makeToken(TOKEN_STAR);

        // deals with out two character lexemes (!=, ==, etc...)
        case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG); // checks to see whether it is just !, or !=
        case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL); // checks to see whether it is ==, or just =
        case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS); // checks to see whether it is <= or just <
        case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER); // checks to see whether it is >= or just >

        case '"': return string(); // if we have a double quote, we call the string function
    }

    return errorToken("Unexpected character."); // otherwise we throw an error...
}