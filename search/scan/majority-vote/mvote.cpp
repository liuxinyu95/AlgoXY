#include <map>
#include <cstdlib>

using namespace std;

/* 
 * Boyer-Moore majority number (vote) problem 
 * [1]. http://www.cs.utexas.edu/~moore/best-ideas/mjrty/
 */

/* A brute force solution. */

template<typename T>
T majority(const T* xs, int n, T fail) {
    map<T, int> m;
    int i, max = 0;
    T r;
    for (i = 0; i < n; ++i)
        ++m[xs[i]];
    for (typename map<T, int>::iterator it = m.begin(); it != m.end(); ++it)
        if (it->second > max) {
            max = it->second;
            r = it->first;
        }
    return max * 2 >= n ? r : fail;
}

void test() {
    const char xs[] = {'A', 'A', 'A', 'C', 'C', 'B', 'B', 'C', 'C', 'C', 'B', 'C', 'C'};
    const int ys[] = {1, 2, 3, 1, 2, 3};
    printf("majority = %c\n", majority(xs, sizeof(xs)/sizeof(xs[0]), 'X'));
    printf("majority = %d\n", majority(ys, sizeof(ys)/sizeof(ys[0]), -1));
}

int main() {
    test();
    return 0;
}
