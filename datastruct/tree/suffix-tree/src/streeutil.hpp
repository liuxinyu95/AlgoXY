//     streeutil.hpp, Suffix Tree utilities
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

#ifndef _SUFFIX_TREE_UTIL_
#define _SUFFIX_TREE_UTIL_

#include <algorithm>
#include <functional>
#include <iterator>

// map (x+) coll in Haskell
// boost lambda: transform(first, last, first, x+_1)
template<class Iter, class T>
void map_add(Iter first, Iter last, T x){
  std::transform(first, last, first, 
                 std::bind1st(std::plus<T>(), x));
}

// x ++ y in Haskell
template<class Coll>
void concat(Coll& x, Coll& y){
  std::copy(y.begin(), y.end(), 
            std::insert_iterator<Coll>(x, x.end()));
}

#endif //_SUFFIX_TREE_UTIL_
