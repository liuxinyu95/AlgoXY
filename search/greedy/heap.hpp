#ifndef _HEAP_
#define _HEAP_

#include <algorithm> //swap

#define top(A) A[0]

unsigned parent(unsigned i){ return ((i+1)>>1)-1; }

unsigned left(unsigned i){ return (i<<1)+1; }

unsigned right(unsigned i){ return (i+1)<<1; }

template<typename Array>
void heapify(Array& a, unsigned i, unsigned n) {
    while (true) {
        unsigned l = left(i), r = right(i), m = i;
        if (l < n && *a[l] < *a[i])
            m = l;
        if (r < n && *a[r] < *a[m])
            m = r;
        if (m != i) {
            std::swap(a[i], a[m]);
            i = m;
        }
        else
            break;
    }
}

template<typename Array>
void build_heap(Array& a, unsigned n) {
    unsigned i = (n - 1) >> 1;
    do {
        heapify(a, i--, n);
    } while (i);
}

template<typename Array>
void pop(Array& a, unsigned n) {
    a[0] = a[n-1];
    heapify(a, 0, --n);
}

template<typename Array>
void fix(Array& a, unsigned i) {
    for (int j = parent(i); i > 0 && *a[i] < *a[j]; i = j, j = parent(i))
        std::swap(a[i], a[j]);
}

template<typename Array, typename T>
void insert(T x, Array& a, unsigned n) {
    a[n] = x;
    fix(a, n);
}

#endif _HEAP_
