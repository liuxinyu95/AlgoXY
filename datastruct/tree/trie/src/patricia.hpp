//     patricia.hpp, Alphabetic Prefix Tree, also known as Patricia
//     Copyright (C) 2010, Liu Xinyu (liuxinyu95@gmail.com)

//     This program is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.

//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.

//     You should have received a copy of the GNU General Public License
//     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef _PATRICIA_
#define _PATRICIA_

#include <iostream>
#include <map>
#include <string>
#include <iterator> //for std::ostream_iterator
#include "trieutil.hpp"

template<class Key, class Value>
struct PrefixTree {
    typedef PrefixTree<Key, Value> Self;
    typedef std::map<Key, Self*> Children;
    typedef Key   KeyType;
    typedef Value ValueType;

    PrefixTree(Value v = Value()) : value(v) {}

    virtual ~PrefixTree() {
        for(typename Children::iterator it=children.begin();
            it!=children.end(); ++it)
            delete it->second;
    }

    Value value;
    Children children;
};

//lcp, extract the longest common prefix,
// it will modify the parameters
template<class K>
K lcp(K& s1, K& s2) {
    typename K::iterator it1(s1.begin()), it2(s2.begin());
    for(; it1!=s1.end() && it2!=s2.end() && *it1 == *it2; ++it1, ++it2);
    K res(s1.begin(), it1);
    s1 = K(it1, s1.end());
    s2 = K(it2, s2.end());
    return res;
}

//branch
template<class T>
T* branch(typename T::KeyType k1, T* t1,
          typename T::KeyType k2, T* t2) {
    if(k1.empty()){ //e.g. insert "an" into "another"
        t1->children[k2] = t2;
        return t1;
    }
    T* t = new T();
    t->children[k1] = t1;
    t->children[k2] = t2;
    return t;
}

template<class K, class V>
PrefixTree<K, V>* insert(PrefixTree<K, V>* t,
                       typename PrefixTree<K, V>::KeyType key,
                       typename PrefixTree<K, V>::ValueType value=V()) {
    if(!t)
        t = new PrefixTree<K, V>();

    PrefixTree<K, V>* p = t;
    typedef typename PrefixTree<K, V>::Children::iterator Iterator;
    for(;;) {
        bool match(false);
        for(Iterator it = p->children.begin(); it!=p->children.end(); ++it) {
            K k=it->first;
            if(key == k) {
                it->second->value = value; //overwrite
                return t;
            }
            K prefix = lcp(key, k);
            if(!prefix.empty()) {
                match=true;
                if(k.empty()){ //e.g. insert "another" into "an"
                    p = it->second;
                    break;
                }
                else{
                    p->children[prefix] = branch(key, new PrefixTree<K, V>(value),
                                                 k, it->second);
                    p->children.erase(it);
                    return t;
                }
            }
        }
        if(!match){
            p->children[key] = new PrefixTree<K, V>(value);
            break;
        }
    }
    return t;
}

template<class K, class V>
V lookup(PrefixTree<K, V>* t, typename PrefixTree<K, V>::KeyType key) {
    typedef typename PrefixTree<K, V>::Children::iterator Iterator;
    if(!t)
        return V(); //or throw exception
    for(;;){
        bool match(false);
        for(Iterator it=t->children.begin(); it!=t->children.end(); ++it) {
            K k = it->first;
            if(key == k)
                return it->second->value;
            K prefix = lcp(key, k);
            if((!prefix.empty()) && k.empty()) {
                match = true;
                t = it->second;
                break;
            }
        }
        if(!match)
            return V(); //or throw exception
    }
}

class PrefixTreeTest {
public:
    void run(){
        std::cout<<"\ntest alphabetic patrica\n";
        test_insert();
        test_lookup();
    }
private:
    template<class Iterator>
    void test_list_to_prefixtree(Iterator first, Iterator last) {
        typedef PrefixTree<std::string, std::string> PrefixTreeType;
        PrefixTreeType* t(0);
        t = list_to_trie(first, last, t);
        std::copy(first, last,
                  std::ostream_iterator<std::string>(std::cout, ", "));
        std::cout<<"\n==>"<<trie_to_str(t)<<"\n";
        delete t;
    }

    void test_insert(){
        const char* lst1[] = {"a", "an", "another", "b", "bob", "bool", "home"};
        test_list_to_prefixtree(lst1, lst1 + sizeof(lst1)/sizeof(char*));

        const char* lst2[] = {"home", "bool", "bob", "b", "another", "an", "a"};
        test_list_to_prefixtree(lst2, lst2 + sizeof(lst2)/sizeof(char*));

        const char* lst3[] = {"romane", "romanus", "romulus"};
        test_list_to_prefixtree(lst3, lst3 + sizeof(lst3)/sizeof(char*));

        typedef PrefixTree<std::string, std::string> PrefixTreeType;
        PrefixTreeType* t(0);
        const char* keys[] = {"001", "100", "101"};
        const char* vals[] = {"y", "x", "z"};
        for(unsigned int i=0; i<sizeof(keys)/sizeof(char*); ++i)
            t = insert(t, std::string(keys[i]), std::string(vals[i]));
        std::copy(keys, keys+sizeof(keys)/sizeof(char*),
                  std::ostream_iterator<std::string>(std::cout, ", "));
        std::cout<<"==>"<<trie_to_str(t)<<"\n";
        delete t;
    }

    void test_lookup() {
        PrefixTree<std::string, int>* t(0);
        const char* keys[] = {"a", "an", "another", "boy", "bool", "home", "an"};
        const int vals[] = {1, 2, 7, 3, 4, 4, -2};
        std::map<std::string, int> m;
        for (unsigned int i = 0; i < sizeof(keys)/sizeof(char*); ++i) {
            t = insert(t, keys[i], vals[i]);
            m[keys[i]] = vals[i];
        }
        for (auto elem : m) {
            std::string key = elem.first;
            int val1 = elem.second;
            int val2 = lookup(t, key);
            std::cout<<"look up("<<key<<")="<<val2<<", "<<(val1 == val2? "OK" : "FAIL")<<"\n";
            if (val1 != val2) {
                exit(-1);
            }
        }
        std::cout<<"lookup tested\n";
        delete t;
    }
};
#endif //_PATRICIA
