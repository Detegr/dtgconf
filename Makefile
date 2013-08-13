OBJ=src/config.o
TESTOBJ=test/configtest.o
CFLAGS=-O2 -std=c99 -Wall -Wextra -D_GNU_SOURCE
CC=gcc
OUT=test/configtest

all: test

test: $(TESTOBJ) $(OBJ)
	-mkdir -p dist
	$(CC) $(CFLAGS) $(TESTOBJ) $(OBJ) -o $(OUT)

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm src/*.o
	-rm test/*.o
	-rm test/configtest

.PHONY: clean
