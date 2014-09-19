#include <string.h>
#include <stdio.h>

#define DELIMITER '#'
#define MAX_LEN 11005

char s[MAX_LEN * 2];
unsigned p[MAX_LEN * 2];

int min(int a, int b) {
    return a < b ? a : b;
}

int manacher_palindrome(char* s) {
    int i, j, m, n = strlen(s);
    for (i = n; i >= 0; --i) {
        s[2*i + 1] = s[i];
        s[2*i] = DELIMITER;
    }
    for (i = j = m = p[0] = 0, n = 2*n + 1; i < n; ++i) {
        p[i] = i < j + p[j]? min(p[2*j - i], j + p[j] - i) : 1;
        while (p[i] <= i && i + p[i] < n && s[i-p[i]] == s[i+p[i]])
            ++p[i];
        j = (j + p[j] < i + p[i]) ? i : j;
        m = m < p[i] ? p[i] : m;
    }
    return m - 1;
}

void test() {
    char* ss[] = {"Mississippi", "level", "cacab", "cocoa", "aaa", "abc"};
    int i;
    for (i = 0; i < sizeof(ss)/sizeof(ss[0]); ++i) {
        strcpy(s, ss[i]);
        printf("longest palindrome for %s, len=%d\n", ss[i], manacher_palindrome(s));
    }
}

void online_judge() {
    while (scanf("%s", s) != EOF)
        if (strlen(s)) printf("%d\n", manacher_palindrome(s));
}

int main(int argc, char** argv) {
    online_judge();
    /*test();*/
    return 0;
}
