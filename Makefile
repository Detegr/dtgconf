OBJ=src/config.o
TESTOBJ=test/configtest.o
EXAMPLEOBJ=example/c/example.o
CFLAGS=-O2 -std=c99 -Wall -Wextra -D_GNU_SOURCE
CC=gcc
TOUT=test/configtest
EOUT=example/c/example

GHC=ghc
HSOBJ=example/haskell/Example bindings/haskell/Config.hs

all: test examples

examples: exc exhs

exc: $(EXAMPLEOBJ) $(OBJ)
	$(CC) $(CFLAGS) $(EXAMPLEOBJ) $(OBJ) -o $(EOUT)

exhs: $(OBJ)
	$(GHC) $(HSOBJ) $(OBJ)

test: $(TESTOBJ) $(OBJ)
	-mkdir -p dist
	$(CC) $(CFLAGS) $(TESTOBJ) $(OBJ) -o $(TOUT)

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm src/*.o
	-rm test/*.o
	-rm test/configtest
	-rm bindings/haskell/*.{hi,o}
	-rm example/*.o
	-rm example/example
	-rm example/haskell/*.{hi,o}
	-rm example/haskell/Example

.PHONY: clean
