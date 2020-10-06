//     trieutil.hpp, Generic utilities for Trie and Patricia Tree.
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

#ifndef _TRIE_UTILITY
#define _TRIE_UTILITY

#include <sstream>
#include <numeric> //for std::accumulate

template<class T>
std::string trie_to_str(T* t, std::string prefix=""){
    std::ostringstream s;
    s<<"("<<prefix;
    if(t->value != typename T::ValueType())
        s<<":"<<t->value;
    for(typename T::Children::iterator it=t->children.begin();
        it!=t->children.end(); ++it)
        s<<", "<<trie_to_str(it->second, prefix+it->first);
    s<<")";
    return s.str();
}

// list_to_trie
template<class Iterator, class T>
T* list_to_trie(Iterator first, Iterator last, T* t){
    return std::accumulate(first, last, t,
                           [] (auto tr, auto key) { return insert(tr, key); });
}

#endif //_TRIE_UTILITY
