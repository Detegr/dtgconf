OBJ=src/config.o
TESTOBJ=test/configtest.o
EXAMPLEOBJ=example/example.o
CFLAGS=-O2 -std=c99 -Wall -Wextra -D_GNU_SOURCE
CC=gcc
TOUT=test/configtest
EOUT=example/example

all: test example

example: $(EXAMPLEOBJ) $(OBJ)
	$(CC) $(CFLAGS) $(EXAMPLEOBJ) $(OBJ) -o $(EOUT)

test: $(TESTOBJ) $(OBJ)
	-mkdir -p dist
	$(CC) $(CFLAGS) $(TESTOBJ) $(OBJ) -o $(TOUT)

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm src/*.o
	-rm test/*.o
	-rm test/configtest
	-rm example/*.o
	-rm example/example

.PHONY: clean
