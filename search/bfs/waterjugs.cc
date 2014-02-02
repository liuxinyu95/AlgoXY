/*
 * waterjugs.cc
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
#include <queue>
#include <list>
#include <algorithm>
#include <cstdio>

using namespace std;

struct Step {
    int p, q;
    Step* parent;
    Step(int x, int y, Step* p = NULL): p(x), q(y), parent(p) {}
};

typedef queue<Step*> Queue;
typedef list<Step*> Steps;

Steps visit;

/** Record in historic data and push */
void push(Queue& q, Step* s) {
    visit.push_back(s);
    q.push(s);
}

Step* pop(Queue& q) {
    Step* s = q.front();
    q.pop();
    return s;
}

Steps expand(Step* s, int a, int b) {
    int p = s->p, q = s->q;
    Steps cs;
    cs.push_back(new Step(a, q, s)); /*fill A*/
    cs.push_back(new Step(p, b, s)); /*fill B*/
    cs.push_back(new Step(0, q, s)); /*empty A*/
    cs.push_back(new Step(p, 0, s)); /*empty B*/
    cs.push_back(new Step(max(0, p + q - b), min(p + q, b), s)); /*pour A into B*/
    cs.push_back(new Step(min(p + q, a), max(0, p + q - a), s)); /*pour B into A*/
    return cs;
}

bool eq(Step* a, Step* b) {
    return a->p == b->p && a->q == b->q;
}

/* Alternatively, this can be implemented with find_if and lambda */
bool visited(Step* s) {
    for (Steps::iterator it = visit.begin(); it != visit.end(); ++it)
        if (eq(*it, s)) return true;
    return false;
}

Step* solve(int a, int b, int g) {
    Queue q;
    push(q, new Step(0, 0));
    while (!q.empty()) {
        Step* cur = pop(q);
        if (cur->p == g || cur->q == g)
            return cur;
        else {
            Steps cs = expand(cur, a, b);
            for (Steps::iterator it = cs.begin(); it != cs.end(); ++it)
                if(!eq(cur, *it) && !visited(*it))
                    push(q, *it);
        }
    }
    return NULL;
}

/**print the steps in reverse order, returns how many steps there are. */
int print(Step* s) {
    int n = -1;
    if (s) {
        n = 1 + print(s->parent);
        printf("%d, %d\n", s->p, s->q);
    }
    return n;
}

void test(int a, int b, int g) {
    printf("solve a=%d, b=%d, g=%d:\n", a, b, g);
    printf("total %d steps\n", print(solve(a, b, g)));
    for (Steps::iterator it = visit.begin(); it != visit.end(); ++it)
        delete *it;
    visit.clear();
}

int main(int argc, char *argv[]) {
    test(3, 5, 4);
    test(4, 9, 6);
    return 0;
}
