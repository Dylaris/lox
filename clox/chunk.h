#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT,      // [opcode] [index]
    OP_NIL,           // [opcode]
    OP_TRUE,          // [opcode]
    OP_FALSE,         // [opcode]
    OP_POP,           // [opcode]
    OP_SET_LOCAL,     // [opcode] [index]
    OP_GET_LOCAL,     // [opcode] [index]
    OP_SET_GLOBAL,    // [opcode] [index]
    OP_GET_GLOBAL,    // [opcode] [index]
    OP_DEFINE_GLOBAL, // [opcode] [index]
    OP_EQUAL,         // [opcode]
    OP_GREATER,       // [opcode]
    OP_LESS,          // [opcode]
    OP_ADD,           // [opcode]
    OP_SUBTRACT,      // [opcode]  
    OP_MULTIPLY,      // [opcode]
    OP_DIVIDE,        // [opcode]
    OP_NOT,           // [opcode]
    OP_NEGATE,        // [opcode]
    OP_PRINT,         // [opcode]
    OP_PRINTLN,       // [opcode]
    OP_JUMP_IF_FALSE, // [opcode] [offset] [offset]
    OP_JUMP,          // [opcode] [offset] [offset]
    OP_LOOP,          // [opcode] [offset] [offset]
    OP_RETURN,        // [opcode]
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t *code;
    int *lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, uint8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif // clox_chunk_h
