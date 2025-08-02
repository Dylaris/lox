#ifndef clox_coimpiler_h
#define clox_coimpiler_h

#include "vm.h"

bool compile(const char *source, Chunk *chunk);

#endif // clox_coimpiler_h
