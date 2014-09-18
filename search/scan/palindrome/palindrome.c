#include <string.h>
#include <stdio.h>

#define DELIMITER '#'
#define MAX_LEN 11000

char s[MAX_LEN * 2 + 5];
unsigned p[MAX_LEN * 2 + 5];

int min(int a, int b) {
    return a < b ? a : b;
}

unsigned manacher_palindrome(char* str) {
    unsigned i, j, m, n = strlen(str);
    for (i = 0; i <= n; ++i) {
        s[2*i] = DELIMITER;
        s[2*i + 1] = str[i];
    }
    for (i = j = m = p[0] = 0, n = 2*n + 1; i < n; ++i) {
        p[i] = 2*j >= i ? min(p[2*j - i], j + p[j] - i) : j + p[j] - i;
        while (p[i] <= i && i + p[i] < n && s[i-p[i]] == s[i+p[i]])
            ++p[i];
        j = (j + p[j] < i + p[i]) ? i : j;
        m = m < p[i] ? p[i] : m;
    }
    return m - 1;
}

void test() {
    char* ss[] = {"Mississippi", "level", "cacab", "cocoa"};
    int i;
    for (i = 0; i < sizeof(ss)/sizeof(ss[0]); ++i)
        printf("longest palindrome for %s, len=%u\n", ss[i], manacher_palindrome(ss[i]));
}

int main(int argc, char** argv) {
    test();
    return 0;
}
