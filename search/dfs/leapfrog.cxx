#include <list>
#include <vector>
#include <algorithm>
#include <cstdio>

using namespace std;

typedef vector<int> State;
typedef list<State> Steps;
typedef list<Steps> Moves;

Steps pop(Moves& stack) {
    Steps top = stack.front();
    stack.pop_front();
    return top;
}

Steps append(Steps steps, State s) {
    steps.push_back(s);
    return steps;
}

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

list<State> solve(State start, State end) {
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

int main(int argc, char** argv) {
    const int from[] = {-1, -1, -1, 0, 1, 1, 1};
    const int to[] = {1, 1, 1, 0, -1, -1, -1};
    State start(from, from + sizeof(from)/sizeof(from[0]));
    State end(to, to + sizeof(to)/sizeof(to[0]));
    Solutions s = solve(start, end);
    printf("total %d solution", s.size());
    return 0;
}
