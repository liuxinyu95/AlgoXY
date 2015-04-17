/*
 * median.c
 * Copyright (C) 2015 Liu Xinyu (liuxinyu95@gmail.com)
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

/*
 * Find the median of two sorted arrays
 *   O(lg(m+n)) time, and O(1) space
 *
 * Suppose the two arrays are A and B.
 * Perform the following binary search first in A then B to find the median.
 *
 * Start from low = 0, high = |A|, guess i = floor (low + high)/2
 * For the median m, there should be total half = floor (|A| + |B| + 1) / 2 elements not greater than it.
 * Since there are i + 1 elements not greater than A[i] in A,
 * There should be half - (i + 1) elements not greater than A[i] in B.
 * Denote j = half - i - 2, thus we can compare if B[j] <= A[i] <= B[j + 1] is satified. This indicates
 * That the guess is correct median.
 * Otherwise, we can easily tell if the guess is too small or too big, then halve the elements to adjust
 * the guess.
 *
 * [1]. MIT, Introduction to algorithm, problem set 9-1.
 * [2]. Dr. Dobb's Finding the Median of Two Sorted Arrays Efficiently.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define N 1000  /*for verification*/

#define min(x, y) (x < y ? x : y)

int odd(int n) { return n & 0x1; }

void swap(int *x, int *y) {
    int tmp = *x; *x = *y; *y = tmp;
}

/* meidan of an array */
double medianof(int A[], int n) {
    return odd(n) ? (double) A[n / 2] : (double)(A[ n / 2] + A[n / 2 - 1]) / 2.0;
}

int find(int A[], int m, int B[], int n) {
    int l = 0, u = m;
    int i, j, half = (m + n + 1) / 2;
    while (l < u) {
        i = (l + u) / 2;
        j = half - i - 2;
        if (j < 0 || j >= n) {
            if (j == -1 && A[i] <= B[0])
                return i; /* found */
            if (j >= n )
                l = i + 1; /* too small */
            else
                u = i; /* too big */
        } else {
            if (B[j]<= A[i] && (j == n - 1 || A[i] <= B[j+1]))
                return i; /* found */
            else if (A[i] < B[j])
                l = i + 1; /* too small */
            else
                u = i; /* too big */
        }
    }
    return -1;
}

double median(int A[], int m, int B[], int n) {
    int i, j, k, *C;
    if (!A || m == 0)
        return medianof(B, n);
    if (!B || n == 0)
        return medianof(A, m);
    if ((i = find(A, m, B, n)) == -1) {
        i = find(B, n, A, m);
        C = A; A = B; B = C;
        swap(&m, &n);
    }
    if (odd(m + n))
        return (double)A[i];
    j = (m + n) / 2 - i - 2;
    if (i == m - 1)
        k = B[j+1];
    else if (j == n - 1)
        k = A[i+1];
    else
        k = min(A[i+1], B[j+1]);
    return (double)(A[i] + k) / 2.0;
}

/*for verification*/
int cmp(const void* x, const void* y) {
    return *(const int*)x - *(const int*)y;
}

void print(int *xs, int n) {
    int i;
    printf("[");
    for (i = 0; i < n; ++i)
        printf(i == n-1 ? "%d]\n" : "%d, ", xs[i]);
}

void check(int *xs, int n, int* ys, int m, double k) {
    double med = median(xs, n, ys, m);
    if (fabs(med - k) > 0.005) {
        printf("FAIL: expected: %f, actual: %f\n", k, med);
        print(xs, n);
        print(ys, m);
        exit(-1);
    }
    //printf("pass\n\n");
}

int main(int argc, char** argv) {
    int c = N, i, j, n, m;
    int xs[N], ys[N];
    double k;
    while(--c) {
        n = rand() % N + 1;
        for (i = 0; i < n; ++i)
            xs[i] = i;
        k = medianof(xs, n);
        for (i = 0; i < n; ++i) {
            j = rand() % n;
            swap(&xs[i], &xs[j]);
        }
        m = rand() % n;
        for (i = 0; i < m; ++i)
            ys[i] = xs[n - 1 - i];
        n -= m;
        qsort(xs, n, sizeof(int), cmp);
        qsort(ys, m, sizeof(int), cmp);
        check(xs, n, ys, m, k);
    }
    return 0;
}
