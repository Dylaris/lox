#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "compiler.h"
#include "scanner.h"
#include "debug.h"

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // > < >= <=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // - !
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
} Local;

typedef struct {
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
Chunk *compilingChunk = NULL;
Compiler *current = NULL;

static void errorAt(Token *token, const char *message)
{
    if (parser.panicMode) return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }
    fprintf(stderr, " : %s\n", message);

    parser.hadError = true;
}

static void errorAtCurrent(const char *message)
{
    errorAt(&parser.current, message);
}

static void error(const char *message)
{
    errorAt(&parser.previous, message);
}

static void advance(void)
{
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break; 
        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char *message)
{
    if (parser.current.type == type) {
        advance();
        return;
    }
    errorAtCurrent(message);
}

static bool check(TokenType type)
{
    return parser.current.type == type;
}

static bool match(TokenType type)
{
    if (!check(type)) return false;
    advance();
    return true;
}

static Chunk *currentChunk(void)
{
    return compilingChunk;
}

static void emitByte(uint8_t byte)
{
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2)
{
    emitByte(byte1);
    emitByte(byte2);
}

static int emitJump(uint8_t instruction) 
{
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn(void)
{
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value)
{
    int constant_index = addConstant(currentChunk(), value);
    if (constant_index > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }
    return (uint8_t)constant_index;
}

static void emitConstant(Value value)
{
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int index)
{
    // -2 to adjust for the bytecode for the jump offset itself
    // Relative offset
    int offset = currentChunk()->count - index - 2;

    if (offset > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    // Big-endian
    currentChunk()->code[index] = (offset >> 8) & 0xff;
    currentChunk()->code[index + 1] = offset & 0xff;
}

static void initCompiler(Compiler *compiler)
{
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static void endCompiler(void)
{
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    disassembleChunk(currentChunk(), "code");
#endif
}

static void beginScope(void)
{
    current->scopeDepth++;
}

static void endScope(void)
{
    current->scopeDepth--;
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void parsePrecedence(Precedence precedence);
static ParseRule *getRule(TokenType type);
static void expression(void);
static void number(bool canAssign);
static void unary(bool canAssign);
static void binary(bool canAssign);
static void grouping(bool canAssign);
static void literal(bool canAssign);
static void variable(bool canAssign);
static void string(bool canAssign);
static void and_(bool canAssign);
static void or_(bool canAssign);

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static ParseRule *getRule(TokenType type)
{
    return &rules[type];
}

static void parsePrecedence(Precedence precedence)
{
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (!prefixRule) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static void expression(void)
{
    parsePrecedence(PREC_ASSIGNMENT);
}

static void number(bool canAssign)
{
    (void)canAssign;

    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign)
{
    (void)canAssign;

    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                    parser.previous.length - 2)));
}

static uint8_t identifierConstant(Token *name)
{
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b)
{
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name)
{
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1;
}

static void addLocal(Token name)
{
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }
    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareVariable(void)
{
    // Just exit if it is global scope.
    if (current->scopeDepth == 0) return;

    Token *name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local *local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static void namedVariable(Token name, bool canAssign)
{
    uint8_t setOp, getOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }


    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign)
{
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign)
{
    (void)canAssign;

    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    case TOKEN_BANG:  emitByte(OP_NOT);    break;
    default: return; // Unreachable.
    }
}

static void binary(bool canAssign)
{
    (void)canAssign;

    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    ParseRule *rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    // Emit the operator instruction.
    switch (operatorType) {
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT);   break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL);            break;
    case TOKEN_GREATER:       emitByte(OP_GREATER);          break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT);    break;
    case TOKEN_LESS:          emitByte(OP_LESS);             break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emitByte(OP_ADD);              break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT);         break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY);         break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE);           break;
    default: return; // Unreachable.
    }
}

static void grouping(bool canAssign)
{
    (void)canAssign;

    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void literal(bool canAssign)
{
    (void)canAssign;

    switch (parser.previous.type) {
    case TOKEN_NIL:   emitByte(OP_NIL);   break;
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_TRUE:  emitByte(OP_TRUE);  break;
    default: return; // Unreachable
    }
}

static void synchronize(void)
{
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

        default:
            ; // do nothing
        }
        advance();
    }
}

static void declaration(void);
static void varDeclaration(void);
static void statement(void);
static void printStatement(void);
static void printlnStatement(void);
static void ifStatement(void);
static void forStatement(void);
static void whileStatement(void);
static void expressionStatement(void);
static void block(void);

// declaration -> varDecl
//              | statement ;
static void declaration(void)
{
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }
    if (parser.panicMode) synchronize();
}

static uint8_t parseVariable(const char *errorMessage)
{
    consume(TOKEN_IDENTIFIER, errorMessage);
    declareVariable();
    if (current->scopeDepth > 0) return 0;
    return identifierConstant(&parser.previous);
}

static void markInitialized(void)
{
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t index)
{
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, index);
}

static void and_(bool canAssign)
{
    (void)canAssign;

    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void or_(bool canAssign)
{
    (void)canAssign;

    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    emitByte(OP_POP);
    patchJump(elseJump);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

// varDecl -> "var" IDENTIFIER ( "=" expression )? ";" ;
static void varDeclaration(void)
{
    uint8_t gvar_index = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(gvar_index);
}

// statement -> exprStmt
//            | printStmt
//            | printlnStmt
//            | ifStmt
//            | forStmt
//            | whileStmt
//            | block ;
static void statement(void)
{
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_PRINTLN)) {
        printlnStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else {
        expressionStatement();
    }
}

static void emitLoop(int loopStart)
{
    emitByte(OP_LOOP);

    // +2 skip the operand of OP_LOOP
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

// forStmt -> "for" "(" ( valDecl | exprStmt | ";" )
//            expression? ";"
//            expression? ")" statement;
static void forStatement(void)
{
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Pop the last condition value from stack.
    }

    endScope();
}

// whileStmt -> "while" "(" expression ")" statement ;
static void whileStatement(void)
{
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    emitLoop(loopStart);
    patchJump(exitJump);

    emitByte(OP_POP); // Pop the last condition value from stack.
}

// ifStmt -> "if" "(" expression ")" statement ( "else" statement )? ;
static void ifStatement(void)
{
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Pop the condition value from stack in then branch.
    statement();
    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);

    emitByte(OP_POP); // Pop the condition value from stack in else branch.
    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

// exprStmt -> expression ";" ; 
static void expressionStatement(void)
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

// printStmt -> "print" expression ";" ;
static void printStatement(void)
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

// printlnStmt -> "println" expression ";" ;
static void printlnStatement(void)
{
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINTLN);
}

// block -> "{" declaration* "}" ;
static void block(void)
{
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

bool compile(const char *source, Chunk *chunk)
{
    // Initialize
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;
    parser.hadError = false;
    parser.panicMode = false;
    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();

    return !parser.hadError;
}
