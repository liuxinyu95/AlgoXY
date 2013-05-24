/*
 * mvote.cpp
 * Copyright (C) 2013 Liu Xinyu (liuxinyu95@gmail.com)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
    return max * 2 > n ? r : fail;
}

/* Boyer-Moore algorithm. */
template<typename T>
T majority(const T* xs, int n, T fail) {
    T m;
    int i, c;
    for (i = 0, c = 0; i < n; ++i) {
        if (!c) 
            m = xs[i];
        c += xs[i] == m ? 1 : -1;
    }
    for (i = 0, c = 0; i < n; ++i, c += xs[i] == m);
    return c * 2 > n ? m : fail;
}

void test() {
    const char xs[] = {'A', 'A', 'A', 'C', 'C', 'B', 'B', 'C', 'C', 'C', 'B', 'C', 'C'};
    const int ys[] = {1, 2, 3, 1, 2, 3};
    const int zs[] = {1, 2, 3, 2, 2, 3, 1, 2, 1, 2, 4, 2};
    printf("majority_brute = %c\n", majority_brute(xs, sizeof(xs)/sizeof(xs[0]), 'X'));
    printf("majority = %c\n", majority(xs, sizeof(xs)/sizeof(xs[0]), 'X'));
    printf("majority_brute = %d\n", majority_brute(ys, sizeof(ys)/sizeof(ys[0]), -1));
    printf("majority = %d\n", majority(ys, sizeof(ys)/sizeof(ys[0]), -1));
    printf("majority_brute = %d\n", majority_brute(zs, sizeof(zs)/sizeof(zs[0]), -1));
    printf("majority = %d\n", majority(zs, sizeof(zs)/sizeof(zs[0]), -1));
}

int main() {
    test();
    return 0;
}
