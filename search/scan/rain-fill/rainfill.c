#include <stdio.h>

/*1D rain-fill solution, O(n).*/
int solve(const int* bs, int n) {
    int l = 0, r = n-1, max_l = bs[l], max_r = bs[r];
    int v = 0;
    while (l < r) {
        if (max_l < max_r) {
            ++l;
            if (bs[l] > max_l)
                max_l = bs[l];
            else
                v += max_l - bs[l];
        } else {
            --r;
            if (bs[r] > max_r)
                max_r = bs[r];
            else
                v += max_r - bs[r];
        }
    }
    return v;
}

int main() {
    const int bs[] = {0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1};
    printf("%d\n", solve(bs, sizeof(bs)/sizeof(bs[0])));
    return 0;
}
