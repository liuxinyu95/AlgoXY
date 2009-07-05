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
