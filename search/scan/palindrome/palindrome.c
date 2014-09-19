/*
 * palindrome.c
 * Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
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
#include <string.h>
#include <stdio.h>

#define DELIMITER '#'
#define MAX_LEN 110005

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
#ifdef ONLINE_JUDGE
    online_judge();
#else
    test();
#endif
    return 0;
}
