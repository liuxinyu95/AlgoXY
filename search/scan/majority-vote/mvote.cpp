#include <map>
#include <cstdlib>

using namespace std;

/* 
 * Boyer-Moore majority number (vote) problem 
 * [1]. http://www.cs.utexas.edu/~moore/best-ideas/mjrty/
 */

/* A brute force solution. */

template<typename T>
T majority_brute(const T* xs, int n, T fail) {
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

/* Boyer-Moore algorithm. */
template<typename T>
T majority(const T* xs, int n, T fail) {
    T m;
    int i, c;
    for (i = 1, m = xs[0], c = 1; i < n; ++i) {
        c += xs[i] == m ? 1 : -1;
        if (c < 0) {
            m = xs[i];
            c = 0;
        }
    }
    for (i = 0, c = 0; i < n; ++i, c += xs[i] == m);
    return c * 2 >= n ? m : fail;
}

void test() {
    const char xs[] = {'A', 'A', 'A', 'C', 'C', 'B', 'B', 'C', 'C', 'C', 'B', 'C', 'C'};
    const int ys[] = {1, 2, 3, 1, 2, 3};
    printf("majority_brute = %c\n", majority_brute(xs, sizeof(xs)/sizeof(xs[0]), 'X'));
    printf("majority = %c\n", majority(xs, sizeof(xs)/sizeof(xs[0]), 'X'));
    printf("majority_brute = %d\n", majority_brute(ys, sizeof(ys)/sizeof(ys[0]), -1));
    printf("majority = %d\n", majority(ys, sizeof(ys)/sizeof(ys[0]), -1));

}

int main() {
    test();
    return 0;
}
