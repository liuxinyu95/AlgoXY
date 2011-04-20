/*
 * nth_elem.cpp
 * Copyright (C) 2011 Liu Xinyu (liuxinyu95@gmail.com)
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
 * 
 */
#include <iostream>
#include <iterator>
#include <vector>

//pivot,a2, a3, ...,left, b1, b2, ..., right, x1, x2, ...
template<typename Iter>
Iter partition(Iter first, Iter last){
    if(std::distance(first, last)<=1)
        return first;

    Iter pivot=first;
    Iter left,right;
    for(left=first, right=first++; right!=last; ++right){
        if(*right<=*pivot){
            std::swap(*left, *right);
            ++left;
        }
    }
    std::swap(*pivot, *--left);
    return left;
}

//return, range: first ~ first+n
template<typename Iter>
void nth_elem(int n, Iter first, Iter last){
    Iter pivot=partition(first, last);
    typedef typename std::iterator_traits<Iter>::difference_type Diff;
    Diff dist=std::distance(first, pivot);
    if(dist==n)
        return;
    else if (dist>n)
        return nth_elem(n, first, pivot);
    else
        return nth_elem(n-dist, pivot, last);
}

void test_partition(){
    const int a[]={4,2,3,1,5,6};
    std::vector<int> coll(a, a+sizeof(a)/sizeof(int));
    std::copy(coll.begin(), coll.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"\n";
    std::vector<int>::iterator p=partition(coll.begin(), coll.end());
    std::copy(coll.begin(), coll.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"\npivot: "<<*p<<"\n";
}

int main(int argc, char** argv){
    //test_partition();
    const int a[]={4,2,3,1,5,6};
    std::vector<int> coll(a, a+sizeof(a)/sizeof(int));
    std::copy(coll.begin(), coll.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"\n";
    nth_elem(3, coll.begin(), coll.end());
    std::copy(coll.begin(), coll.begin()+3, std::ostream_iterator<int>(std::cout, ", "));
    std::cout<<"\n";
}
