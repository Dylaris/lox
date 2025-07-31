#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT, // [opcode] [constant_index]
    OP_ADD,      // [opcode]
    OP_SUBTRACT, // [opcode]  
    OP_MULTIPLY, // [opcode]
    OP_DIVIDE,   // [opcode]
    OP_NEGATE,   // [opcode]
    OP_RETURN,   // [opcode]
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
