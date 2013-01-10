#include <iostream>
#include <algorithm>

template<typename T>
T& min(T* from, T* to) {
    T* m;
    for (m = from++; from != to; ++from)
        if (*from < *m)
            m = from;
    return *m;
}

template<typename T>
void naive_ssort(T* xs, int n) {
    int i;
    for (i = 0; i < n; ++i)
        std::swap(xs[i], min(xs+i, xs+n));
}

int main(int, char**) {
    unsigned i;
    int xs[] = {3, 1, 2, 4, 0};
    naive_ssort(xs, sizeof(xs)/sizeof(xs[0]));
    for (i = 0; i < sizeof(xs)/sizeof(xs[0]); ++i)
        printf("%d, ", xs[i]);
    printf("\n");
    return 0;
}
