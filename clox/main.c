#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(void)
{
    Chunk chunk;
    initChunk(&chunk);

    int constant_index = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant_index, 123);

    writeChunk(&chunk, OP_RETURN, 124);

    disassembleChunk(&chunk, "test chunk");
    freeChunk(&chunk);

    return 0;
}
