#include <string.h>

#include "scanner.h"
#include "common.h"

typedef struct {
    const char *start;
    const char *current;
    int line;
} Scanner;

Scanner scanner;

void initScanner(const char *source)
{
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAtEnd(void)
{
    return *scanner.current == '\0';
}

static char advance(void)
{
    return *scanner.current++;
}

static bool match(char expected)
{
    if (isAtEnd()) return false;
    if (*scanner.current != expected) return false;
    scanner.current++;
    return true;
}

static char peek(void)
{
    return *scanner.current;
}

static char peekNext(void)
{
    if (isAtEnd()) return '\0';
    return scanner.current[1];
}

static void skipWhitespace(void)
{
    for (;;) {
        char c = peek();
        switch (c) {
        case ' ':
        case '\t':
        case '\r':
            advance();
            break;
        case '\n':
            scanner.line++;
            advance();
            break;
        case '/':
            if (peekNext() == '/') {
                while (peek() != '\n' && !isAtEnd()) advance();
            } else {
                return;
            }
            break;
        default:
            return;
        }
    }
}

static Token makeToken(TokenType type)
{
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char *message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = scanner.line;
    return token;
}

static bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

static bool isAlpha(char c)
{
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c == '_');
}

static Token string(void)
{
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') scanner.line++;
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    // The closing '"'
    advance();
    return makeToken(TOKEN_STRING);
}

static Token number(void)
{
    while (isDigit(peek())) advance();

    // Look for a fractional part
    if (peek() == '.' && isDigit(peekNext())) {
        advance(); // consume '.'
        while (isDigit(peek())) advance();
    }

    return makeToken(TOKEN_NUMBER);
}

static TokenType checkKeyword(int start, int length,
        const char *rest, TokenType type)
{
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

static TokenType identifierType(void)
{
    switch (scanner.start[0]) {
    case 'a': return checkKeyword(1, 2, "nd",    TOKEN_AND);
    case 'c': return checkKeyword(1, 4, "lass",  TOKEN_CLASS);
    case 'e': return checkKeyword(1, 3, "lse",   TOKEN_ELSE);
    case 'f':
        if (scanner.current - scanner.start > 1) {
            switch (scanner.start[1]) {
                case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                case 'u': return checkKeyword(2, 1, "n",   TOKEN_FUN);
                case 'o': return checkKeyword(2, 1, "r",   TOKEN_FOR);
            }
        } 
        break;
    case 'i': return checkKeyword(1, 1, "f);",   TOKEN_IF);
    case 'n': return checkKeyword(1, 2, "il",    TOKEN_NIL);
    case 'o': return checkKeyword(1, 1, "r);",   TOKEN_OR);
    case 'p': return checkKeyword(1, 4, "rint",  TOKEN_PRINT);
    case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
    case 's': return checkKeyword(1, 4, "uper",  TOKEN_SUPER);
    case 't':
        if (scanner.current - scanner.start > 1) {
            switch (scanner.start[1]) {
                case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
                case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
            }
        } 
        break;
    case 'v': return checkKeyword(1, 2, "ar",    TOKEN_VAR);
    case 'w': return checkKeyword(1, 4, "hile",  TOKEN_WHILE);
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier(void)
{
    while (isAlpha(peek()) || isDigit(peek())) advance();
    return makeToken(identifierType());
}

Token scanToken(void)
{
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    char c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();
    switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '"': return string();
    }

    return errorToken("Unexpected character.");
}

