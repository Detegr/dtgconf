OBJ=src/config.o
TESTOBJ=test/configtest.o
EXAMPLEOBJ=example/c/example.o
EXAMPLEOBJCPP=bindings/cpp/config.o example/cpp/example.o
CFLAGS=-O2 -std=c99 -Wall -Wextra -D_GNU_SOURCE
CXXFLAGS=-O2 -Wall -Wextra -std=c++11
CC=gcc
CXX=g++
TOUT=test/configtest
EOUT=example/c/example
ECPPOUT=example/cpp/example

GHC=ghc
HSOBJ=example/haskell/Example bindings/haskell/Config.hs

all: test examples

examples: exc excpp exhs

exc: $(EXAMPLEOBJ) $(OBJ)
	$(CC) $(CFLAGS) $(EXAMPLEOBJ) $(OBJ) -o $(EOUT)

excpp: $(EXAMPLEOBJCPP) $(OBJ)
	$(CXX) $(CXXFLAGS) $(EXAMPLEOBJCPP) $(OBJ) -o $(ECPPOUT)

exhs: $(OBJ)
	$(GHC) $(HSOBJ) $(OBJ)

test: $(TESTOBJ) $(OBJ)
	-mkdir -p dist
	$(CC) $(CFLAGS) $(TESTOBJ) $(OBJ) -o $(TOUT)

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

%.o : %.cpp %.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	-rm src/*.o
	-rm test/*.o
	-rm test/configtest
	-rm bindings/haskell/*.{hi,o}
	-rm bindings/cpp/*.o
	-rm example/c/*.o
	-rm example/example
	-rm example/haskell/*.{hi,o}
	-rm example/haskell/Example
	-rm example/cpp/*.o
	-rm example/cpp/example

.PHONY: clean
