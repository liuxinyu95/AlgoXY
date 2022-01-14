HEADER = 
SOURCE = mtf.c
CFLAGS = -Wall
#CXX_FLAGS = -I/usr/include/boost-1_33_1
COBJ = mtf

.PHONY: all 

all: c

c: $(HEADER)
	gcc $(CFLAGS) $(SOURCE) -o $(COBJ)

clean:
	rm -f *.o main rbt *.pyc *.hi *.4ct *.4tc *.aux *.css *.dvi *.html 
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *~
