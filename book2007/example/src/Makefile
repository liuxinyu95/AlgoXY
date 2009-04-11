TEST_FW = ../inc
CXXFLAGS = -I$(TEST_FW)
#LDFLAGS = =L($LIB_HOME)/lib \
		-lxxx -lcppunit

HEADERS = evaluator.h lambda_demo.h lambda.h env.h expr.h proc.h util.h lexer.h parser.h stream.h
#LIBOBJS = lib1.o lib2.o lib3.o
CXXOBJS = lambda.o equality.o main.o evaluator.o env.o stream.o
PLISOBJS= main.o evaluator.o env.o
STREAMOBJS = main.o stream.o

#LIB_DIR = inc/test
#INC_DIR = inc/test

.PHONY: all

all: compile

compile: $(CXXOBJS)
	@echo 'building target: $@ ...'
	g++ $(CXXOBJS) -o ../example

plis: $(PLISOBJS)
	@echo 'building target: $@ ...'
	g++ $(PLISOBJS) -o ../example

stream: $(STREAMOBJS)
	@echo 'building target: $@ ...'
	g++ $(STREAMOBJS) -o ../example

$(CXXOBJS): %o: %cpp $(HEADERS)
	@echo 'building target: $@ ...'
	g++ -c $< $(CXXFLAGS) -o $@

clean:
	rm -f $(CXXOBJS)
	rm -f *.o
	rm -f ../example
