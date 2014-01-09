#include <queue>
#include <list>
#include <iostream>
#include <algorithm>
#include <iterator>

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

void print_steps(Steps s) {
    cout<<"(";
    copy(s.begin(), s.end(), ostream_iterator<unsigned>(cout, ", "));
    cout<<") ";
}

void print_queue(Queue q) {
    cout<<"[";
    while (!q.empty())
        print_steps(pop(q));
    cout<<"]\n";
}

bool invalid(const Steps& s, int x) {
    unsigned a = x>>4, b = x & 0x0f;
    return a == 3 || a == 6 || b == 3 || b == 6 || 
        find(s.begin(), s.end(), x) != s.end();
}

unsigned state(unsigned a, unsigned b) {
    return (a << 4) + b;
}

Steps moves(const Steps& s) {
    unsigned a = s.back() >> 4, b = s.back() & 0x0f;
    Steps ms;
    int i, mask, x;
    for (i = 0; i < 4; ++i) {
        mask = 8 | (1 << i);
        if ((a & mask) == mask && (!invalid(s, x = state(a ^ mask, b | mask))))
            ms.push_back(x);
        if ((b & mask) == mask && (!invalid(s, x = state(a | mask, b ^ mask))))
            ms.push_back(x);
    }
    return ms;
}

Solution solve() {
    Solution s;
    Queue q;
    q.push(Steps(1, 0xf0));
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

void print_set(unsigned s) {
    int i;
    const char* ns[] = {"wolf", "goat", "cabbage", "farmer"};
    cout<<"[";
    for (i = 0; i < 4; ++i)
        if (s & (1 << i))
            cout<<ns[i]<<" ";
    cout<<"]";
}

void print_move(unsigned x) {
    print_set(x >> 4);
    cout<<"====";
    print_set(x & 0xf);
    cout<<"\n";
}

void test() {
    Steps::iterator it;
    Solution s = solve();
    for(Solution::iterator is = s.begin(); is != s.end(); ++is) {
        for (Steps::iterator it = is->begin(); it != is->end(); ++it)
            print_move(*it);
        cout<<"total "<<is->size()<<" steps.\n\n";
    }
    cout<<"total "<<s.size()<<" solutions.\n";
}

int main(int, char**) {
    test();
    return 0;
}
