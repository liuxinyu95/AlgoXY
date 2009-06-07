.PHONY: all

all: particlesys

particlesys:
	g++ particlesys.cpp -Wall -o main

clean:
	rm *.o *.exe
	rm main

boost:
	g++ -I /usr/include/boost-1_33_1 boost_test.cpp -o boost_test
