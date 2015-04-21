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
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*
 * Find the meidan of two sorted arrays
 * For arrays, A of length m, and B of length n (index starts from 1)
 * Find the median by using the k-th element method.
 * Where
 *    median = k-th(A, B, k), k = floor(m + n) / 2 + 1) if m + n is odd
 *    median = (k-th(A, B, k) + k-th(A, B, k-1)) / 2 if m + n is even
 *
 * In order to find the k-th element in A, and B
 * Suppose m >= n, (exchange A, B otherwise)
 * If B is empty, return the k-th element in A;
 * If k = 1, return the minimum of A[1] and B[1]
 * Otherwise, guess j = min(k/2, n), and i = k - j
 * then compare A[i] and B[j]
 * If A[i] < B[j], Drop all elements before A[i] and after B[j], then
 * recursively find the (k - i)-th element in the rest.
 * Othewise, Drop all elements before B[j] and after A[i], then recursively
 * find the (k-j)-th element in the rest.
 */

#define N 10
#define min(a, b) (a < b ? a : b)

int kth(int A[], int m, int B[], int n, int k) {
    int i, j;
    if (m < n) return kth(B, n, A, m, k);
    if (n == 0) return A[k - 1];
    if (k == 1) return min(A[0], B[0]);
    j = min(k/2, n);
    i = k - j;
    return A[i - 1] < B[j - 1] ?
        kth(A + i, m - i, B, j, k - i):
        kth(A, i, B + j, n - j, k - j);
}

double median(int A[], int m, int B[], int n) {
    int k = (m + n) / 2;
    double x = (double) kth(A, m, B, n, k + 1);
    if (!((m + n) & 0x1)) /* even */
        x = (x + (double) kth(A, m, B, n, k) / 2.0;
    return x;
}

/*for verification*/
int cmp(const void* x, const void* y) {
    return *(const int*)x - *(const int*)y;
}

void swap(int *a, int *b) {
    int tmp = *a; *a = *b; *b = tmp;
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
}

int main(int argc, char** argv) {
    int i, j, n, m, c = N;
    int xs[N], ys[N];
    double k;
    while(--c) {
        n = rand() % N + 1;
        for (i = 0; i < n; ++i)
            xs[i] = i;
        k = (double)(n - 1) / 2.0;
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
