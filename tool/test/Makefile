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
CHEADER = randlist.h
CSOURCE = maintest.c
C_TEST_OBJS = maintest.o
CC = gcc -Wall
CFLAGS =

# Objective-C part
#
# Note, in order to pass GNUstep compile, the following modification is needed:
# file: /usr/include/GNUstep/GNUstepBase/GSConfig.h
#   before: #define BASE_NATIVE_OBJC_EXCEPTIONS   1
#   after : #define BASE_NATIVE_OBJC_EXCEPTIONS   0
#
# And the OBJCFLAGS should as below, may `-lm' need.

OBJC_HEADER =
OBJC_SOURCE = foo.m
OBJC_OBJ = foo
OBJC_FLAGS = -I /usr/include/GNUstep/ -L /usr/lib/GNUstep/ -lgnustep-base -lz -fconstant-string-class=NSConstantString

objc: $(OBJC_HEADER)
	gcc $(OBJC_FLAGS) $(OBJC_SOURCE) -o $(OBJC_OBJ)

# C++ part
CXX_HEADER = 
CXX_SOURCE =
CPP_TEST_OBJS = test.o
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
