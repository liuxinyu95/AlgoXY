HEADER = inttrie.hpp intpatricia.hpp trie.hpp patricia.hpp trieutil.hpp edict.hpp
SOURCE = trie.cpp
CPP_TEST_OBJS = test.o
CXX = g++ -Wall -I/usr/include/boost-1_33_1
CXX_FLAGS = 
HS_TEST_SRCS = IntTrie.hs IntPatricia.hs Trie.hs Patricia.hs TestMain.hs
HS_TEST_OBJS = $(HS_TEST_SRCS:.hs=.o)
HS_APP_SRCS = EDict.hs
HS_APP_OBJS = EDict.o Trie.o Patricia.o

.SUFFIXES: .o .hs

.PHONY: all

all: cpp haskell

cpp: cpptest

cpptest: $(CPP_TEST_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CPP_TEST_OBJS)

$(CPP_TEST_OBJS): $(HEADER)

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