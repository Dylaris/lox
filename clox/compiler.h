#ifndef clox_coimpiler_h
#define clox_coimpiler_h

#include "vm.h"
#include "object.h"

ObjFunction *compile(const char *source);
void markCompilerRoots(void);

#endif // clox_coimpiler_h
