#include <iostream>
#include <list>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

template<class Coll>
void print(const Coll& coll){
    copy(coll.begin(),coll.end(), ostream_iterator<typename Coll::value_type>(cout, ", "));
    cout<<"\n";

}

template<class Coll, class T>
Coll map_cons(T x, Coll coll){
    Coll res;
    if(coll.empty())
        res.push_back(string(1, x));
    else
        transform(coll.begin(), coll.end(),
              insert_iterator<Coll>(res, res.begin()),
              bind1st(plus<string>(), string(1, x)));
    return res;

}

template<class Coll>
void append(Coll& a, Coll b){
    copy(b.begin(), b.end(), insert_iterator<Coll>(a, a.end()));
}

list<string> f(vector<string> params, string pattern/*AABBA...*/){
    list<string> res;
    if(!pattern.empty()){
        int selector = *pattern.begin() - 'A';
        string enum_set = params[selector];
        for(string::iterator i=enum_set.begin(); i!=enum_set.end(); ++i){
            vector<string> new_param(params);
            new_param[selector].erase(find(new_param[selector].begin(), new_param[selector].end(), *i));
            append(res, map_cons(*i, f(new_param, string(pattern.begin()+1, pattern.end()))));
        }
    }
    return res;
}

int main(int, char**){
    vector<string> candidates;
    candidates.push_back(string("abcd"));
    candidates.push_back(string("123"));
    print(f(candidates, "ABBA"));
} 
