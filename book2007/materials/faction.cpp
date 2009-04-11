#include <iostream>

typedef int (*F)(int, int);

int faction(int n, int f){
    if(n == 0)
        return 1;
    else
        return n* reinterpret_cast<F>(f)( n-1, f);
}

int main(int, char**){
    std::cout<<"6!="<<faction(6, reinterpret_cast<int>(faction))<<"\n";
}

