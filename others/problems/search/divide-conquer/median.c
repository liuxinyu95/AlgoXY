#include <stdlib.h>
#include <stdio.h>
#include <math.h>

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
    if (!((m + n) & 0x1)) /*odd*/
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
