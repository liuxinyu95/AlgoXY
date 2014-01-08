#include <queue>
#include <list>

using namespace std;

typedef list<unsigned> Steps;
typedef queue<Steps> Queue;
typedef list<Steps> Solution;

Steps pop(Queue& q) {
    Steps f = q.front();
    q.pop();
    return f;
}

Steps append(Steps steps, unsigned s) {
    steps.push_back(s);
    return steps;
}

Steps moves(Steps s) {
    unsigned a = s.back() & 0xf0, b = s.back() & 0x0f;
    
}

Solution solve() {
    Solution s;
    Queue q(Steps(1, 0xf0));
    while (!q.empty()) {
        Steps cur = pop(q);
        if (cur.back() == 0x0f)
            s.push_back(cur);
        else {
            Steps cs = moves(cur);
            for (Steps::iterator it = cs.begin(); it != cs.end(); ++it)
                q.push(append(cur, *it));
        }
    }
    return s;
}
