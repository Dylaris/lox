CC 	   = gcc
CFLAGS = -Wall -Wextra -ggdb -std=c99
TARGET = main
SRC    = main.c chunk.c memory.c debug.c value.c \
		 vm.c compiler.c scanner.c object.c table.c
OBJ    = $(SRC:.c=.o)

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) -o $(TARGET) $(OBJ) 

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf $(OBJ) $(TARGET)

.PHONY: all clean

