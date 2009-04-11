CXXFLAGS  = -I$(SPIDER_HOME)/include
CFLAGS	  = -L$(SPIDER_HOME)/lib
#LDFLAGS   =	-L$(SPIDER_HOME)/lib \
#			-linfralogging -ljailproxylib -ljail \
#			-lafw -lutilmailbox -lutilcommons\
#			-lmico2.3.11 -lcppunit
LDFLAGS   =	-L$(SPIDER_HOME)/lib \
			-lcppunit

HEADERS	  = jal_common.h jal.h jal_error.h jal_sys.h jal_login.h jal_mailbox.h 
			#jal_document.h
LIBOBJS	  = jal.o jal_login.o jal_mailbox.o 
			#jal_document.o
CXXOBJS	  = jal_test.o
COBJS 	  = jal_ctest.o

LIB_DIR   = ../lib/
BIN_DIR   = ../bin/
INC_DIR   = ../include/

.PHONY:	all

all: jal_run jal_lib jal_ctest

jal_lib: $(LIBOBJS)
	@echo 'Building target: $@ ...'
	mkdir -p $(LIB_DIR)
	mkdir -p $(BIN_DIR)
	mkdir -p $(INC_DIR)
	g++ $(LIBOBJS) $(LDFLAGS) -fPIC -shared -o ../lib/libjal.so
	cp -f ../lib/libjal.so $(SPIDER_HOME)/lib
	cp -f $(HEADERS) ../include
	cp -f ../include/*.h $(SPIDER_HOME)/include
	@echo 'Finished building: $@'

jal_run: $(CXXOBJS) jal_lib
	@echo 'building target: $@ ...'
	mkdir -p $(BIN_DIR)
	g++ $(CXXOBJS) $(LIBOBJS) $(LDFLAGS) -ljal -ldl -o ../bin/jal_test
	@echo 'finished buiding target: $@'
	
jal_ctest: $(COBJS) $(CDLLOBJ) jal_lib
	@echo 'building target: $@ ...'
	mkdir -p $(BIN_DIR)
	gcc $(COBJS) $(CFLAGS) $(LDFLAGS) -ljal -ldl -o ../bin/jal_ctest
	gcc jal_ctest.c  -D__USE_DLL__ $(CFLAGS) $(LDFLAGS) -ldl -o ../bin/jal_cdlltest
	@echo 'finished building target: $@'

$(LIBOBJS): %.o: %.cpp $(HEADERS)
	@echo 'building target: $@ ...'
	g++ -c $< $(CXXFLAGS) -o $@

$(CXXOBJS): %.o: %.cpp $(HEADERS)
	@echo 'building target: $@ ...'
	g++ -c $< $(CXXFLAGS) -o $@

$(COBJS): %.o: %.c jal.h
	gcc -c $< $(CFLAGS) -o $@	
	
clean:
	rm -f $(LIBOBJS) $(CXXOBJS) $(COBJS)
	rm -f *.o
	rm -f ../bin/jal*
	rm -f ../lib/lib*
	rm -f libjal.so
	rm -f ../include/*.h
	#rm -f $(SPIDER_HOME)/lib/libjal.so

	