#     Makefile for test tool
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
CSOURCE = 
C_TEST_OBJS = 
CC = gcc -Wall
CFLAGS = -I../../../tool/test

# C++ part
CXX_HEADER = 
CXX_SOURCE = list.cpp
CPP_TEST_OBJS = list.o
CXX_TMP_SRC = list-tmp.cpp
CXX_TMP_OBJS = list-tmp.o
CXX_FP_SRC = list-imp.cpp
CXX_FP_OBJS = list-imp.o
CXX = g++ -Wall -I/usr/include/boost-1_33_1
CXX_FLAGS =

# Haskell part 
HS_TEST_SRCS = Foo.hs
HS_TEST_OBJS = $(HS_TEST_SRCS:.hs=.o)
HS_APP_SRCS = Bar.hs
HS_APP_OBJS = Bar.o Foo.o

.SUFFIXES: .o .hs

.PHONY: all

all: c lib cpp haskell

c: ctest

ctest: $(C_TEST_OBJS)
	$(CC) $(CFLAGS) -o $@ $(C_TEST_OBJS)

$(C_TEST_OBJS): $(CHEADER)

cpp: list list-imp list-tmp

list: $(CPP_TEST_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CPP_TEST_OBJS)

list-tmp: $(CXX_TMP_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CXX_TMP_OBJS)

list-imp: $(CXX_FP_OBJS)
	$(CXX) $(CXX_FLAGS) -o $@ $(CXX_FP_OBJS)

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
	rm -f *.exe *.hi *~ *.o *.pyc list list-imp list-tmp
