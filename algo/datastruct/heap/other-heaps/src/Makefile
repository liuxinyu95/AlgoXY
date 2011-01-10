#     Makefile for C++, C, and Haskell programs
#     Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

HEADER = xxx.hpp
SOURCE = xxx.cpp
CPP_TEST_OBJS = xxx.o
CXX = g++ -Wall -ggdb -I/usr/include/boost-1_33_1
CXX_FLAGS = 
#HS_TEST_SRCS = BinomialHeap.hs FibonacciHeap.hs PairingHeap.hs
#HS_TEST_OBJS = $(HS_TEST_SRCS:.hs=.o)
HS_TOOL_SRCS = Fr2Dot.hs
HS_TOOL_OBJS = Fr2dot.o

.SUFFIXES: .o .hs

.PHONY: all

all: cpp haskell

cpp: cpptest

cpptest: $(CPP_TEST_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CPP_TEST_OBJS)

$(CPP_TEST_OBJS): $(HEADER)

c:
	gcc trie.c -o triec

haskell: htest tool

# Makefile auto variable, $@, target; $< first prerequiste
htest : $(HS_TEST_OBJS)
	ghc -o $@ $(HS_TEST_OBJS)

#ghc --make 
fr2dot: $(HS_TOOL_OBJS)
	ghc -o $@ $(HS_TOOL_SRCS) --make

tool: fr2dot

# Standard suffix rules
%.o : %.hs
	ghc -c $<

clean:
	rm -f *.exe *.hi *~ *.o *.pyc