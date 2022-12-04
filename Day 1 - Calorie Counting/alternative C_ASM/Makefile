# Add additional compiler flags here
CC=gcc

CFLAGS=-O0 -Wall -Wextra

all: calorie
calorie: main.c asm.S
	$(CC) $(CFLAGS) -o $@ $^
clean:
	rm -f calorie