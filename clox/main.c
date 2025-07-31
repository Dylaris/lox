#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(void)
{
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    int constant_index;

    constant_index = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant_index, 123);

    constant_index = addConstant(&chunk, 3.4);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant_index, 123);

    writeChunk(&chunk, OP_ADD, 123);

    constant_index = addConstant(&chunk, 5.6);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant_index, 123);

    writeChunk(&chunk, OP_DIVIDE, 123);
    writeChunk(&chunk, OP_NEGATE, 123);

    writeChunk(&chunk, OP_RETURN, 124);

    disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);

    freeVM();
    freeChunk(&chunk);

    return 0;
}
