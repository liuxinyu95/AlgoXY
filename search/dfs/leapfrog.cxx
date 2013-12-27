#include <list>
#include <cstdio>

#using namespace std;

typedef list<int> State;
typedef list<State> Steps;
typedef list<Steps> Moves;

list<State> solve(State start, State end) {
    Moves s, stack(1, start);
    Steps cur, attemps;
    while (!stack.empty()) {
        if (stack.front().back() == end) {
            s.push_back(stack.front());
            stack.pop_front();
        } else {
            Steps cur = stack.front();
            stack.
            cur.back();
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
