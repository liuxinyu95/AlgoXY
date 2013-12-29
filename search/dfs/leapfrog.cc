/*
 * leapfrog.cc
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
#include <list>
#include <vector>
#include <algorithm>
#include <iostream>
#include <iterator>

using namespace std;

/* 
 * As the leap/hop are realized by swapping, vector is used to realize
 * random access. New attemptation is append on the tail of steps, while
 * stack is manipulate on the head, list can serve both.
 */
typedef vector<int> State;
typedef list<State> Steps;
typedef list<Steps> Moves;

/* A combined operation, peek the front and extract it from the stack. */
Steps pop(Moves& stack) {
    Steps top = stack.front();
    stack.pop_front();
    return top;
}

/* 
 * The 'passed by value' helps creating a new copy of steps. The new 
 * attemptation is appended to it. The new copy is returned.
 */
Steps append(Steps steps, State s) {
    steps.push_back(s);
    return steps;
}

/*
 * The 'passed by value' helps duplicating a new state, then swaps
 * to realize leap/hop, and returns this new attemptation.
 */
State swap(State s, int i, int j) {
    swap(s[i], s[j]);
    return s;
}

list<State> moves(State s) {
    list<State> ms;
    int n = s.size(), p = find(s.begin(), s.end(), 0) - s.begin();
    if (p < n - 2 && s[p+2] > 0)  /* leap left */
        ms.push_back(swap(s, p, p+2));
    if (p < n - 1 & s[p+1] > 0)   /* hop left */
        ms.push_back(swap(s, p, p+1));
    if (p > 1 && s[p-2] < 0)     /* leap right*/
        ms.push_back(swap(s, p, p-2));
    if (p > 0 && s[p-1] < 0)     /* hop right */
        ms.push_back(swap(s, p, p-1));
    return ms;
}

/* The DFS solution */
Moves solve(State start, State end) {
    Moves s, stack(1, Steps(1, start));
    while (!stack.empty()) {
        Steps cur = pop(stack);
        if (cur.back() == end)
            s.push_back(cur);
        else {
            list<State> attempts = moves(cur.back());
            list<State>::iterator it;
            for (it = attempts.begin(); it!=attempts.end(); ++it)
                stack.push_front(append(cur, *it));
        }
    }
    return s;
}

/* Auxiliary functions for output and verification. */

void print_line(State s) {
    copy(s.begin(), s.end(), ostream_iterator<int>(cout, ", "));
    cout<<"\n";
}

void print(Moves s) {
    int i, n;
    Moves::iterator itm;
    Steps::iterator its;
    for (n = 0, itm = s.begin(); itm != s.end(); ++itm, ++n) {
        cout<<"solution "<< n + 1<<":\n";
        for (i = 0, its = itm->begin(); its != itm->end(); ++its, ++i)
            print_line(*its);
        cout<<"total "<<i - 1<<" steps.\n\n";
    }
    cout<<"total "<<n<<" solutions.\n";
}

void test() {
    for (int i = 1; i < 6; ++i) {
        State from(2*i+1, 0), to(2*i+1, 0);
        for(int j = 0; j < i; ++j) {
            from[j] = to[j+i+1] = -1;
            to[j] = from[j+i+1] = 1;
        }
        print(solve(from, to));
    }
}

int main(int argc, char** argv) {
    test();
    return 0;
}
