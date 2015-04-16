/*Find the median of two sorted arrays*/
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
    if (!A || m == 0)
        return medianof(B, n);
    if (!B || n == 0)
        return medianof(A, m);
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
