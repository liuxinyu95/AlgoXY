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

# C part
CHEADER = 
CSOURCE = insertsort
C_TEST_OBJS = insertsort.o
CC = gcc -Wall -g
CFLAGS = -I../../../tool/test

# C++ part
CXX_HEADER = 
CXX_SOURCE = ssort.cpp
CPP_TEST_OBJS = ssort.o
CXX = g++ -Wall -I/usr/include/boost-1_33_1
CXX_FLAGS =

# Haskell part 
HS_TEST_SRCS = 
HS_TEST_OBJS = $(HS_TEST_SRCS:.hs=.o)
HS_APP_SRCS = Fr2dot.hs
HS_APP_OBJS = Fr2dot.o

.SUFFIXES: .o .hs

.PHONY: all

all: c lib cpp haskell

c: insertsort

ctest: $(C_TEST_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_TEST_OBJS)

$(C_TEST_OBJS): $(CHEADER)

cpp: cpptest

cpptest: $(CPP_TEST_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CPP_TEST_OBJS)

$(CPP_TEST_OBJS): $(HEADER)

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
