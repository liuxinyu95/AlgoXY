HEADER = 
SOURCE = trie.cpp
HS_TEST_SRCS = IntTrie.hs IntPatricia.hs Trie.hs Patricia.hs TestMain.hs
HS_TEST_OBJS = $(HS_TEST_SRCS:.hs=.o)
HS_APP_SRCS = EDict.hs
HS_APP_OBJS = EDict.o Trie.o
#CXX_FLAGS = -I/usr/include/boost-1_33_1

.SUFFIXES: .o .hs

.PHONY: all

all: cpp haskell

cpp: $(HEADER)
	g++ $(CXX_FLAGS) $(SOURCE) -o main

c:
	gcc trie.c -o triec

haskell: htest happ

# Makefile auto variable, $@, target; $< first prerequiste
htest : $(HS_TEST_OBJS)
	ghc -o $@ $(HS_TEST_OBJS)

#ghc --make EDict -o edict
happ: $(HS_APP_OBJS)
	ghc -o $@ $(HS_APP_OBJS)

# Standard suffix rules
%.o : %.hs
	ghc -c $<

clean:
	rm -f *.exe *.hi *~ *.o *.pyc