#include <list>
#include <string>
#include <iostream>
 
using namespace std;
 
struct chunk{
    chunk(void* p, bool isArray=false): ptr(p), isArray(isArray){}
    chunk(const chunk& v):ptr(v.ptr), isArray(v.isArray){}
    chunk& operator = (const chunk& v){
		ptr = v.ptr;  isArray = v.isArray;
		return *this;
	}
 
	const bool operator == (const chunk& v){ return ptr == v.ptr; }
 
	void* ptr;
	bool  isArray;
};
 
class gc_obj{
public:
	gc_obj():_info("noname"){ cout<<_info<<endl;}
	gc_obj(string objName):_info(objName){ cout<<_info<<endl;}
	virtual ~gc_obj(){}
 
	static void* operator new(size_t size);
	static void* operator new[](size_t size);
	static void  operator delete(void* p);
	static void  operator delete[](void*p);
private:
	string _info;
};
 
class gc{
private:
	gc(){}
	gc(const gc&);
	gc& operator = (const gc&);
 
	list<chunk> mem_list;
public:
	static gc& instance(){
		static gc inst;
		return inst;
	}
 
	~gc();
 
	void* alloc(size_t size, bool isArray = false){
		gc_obj* p = (gc_obj*) (isArray?
						  ::operator new[](size) :
						  ::operator new(size));
		mem_list.push_back(chunk(p, isArray));
		cout<<"\talloc by gc at: "<<hex<<p<<dec<<" for ";
		return p;
	}
 
	void free(void* p, bool isArray = false){
		cout<<"\tfree by gc\n";
		mem_list.remove(chunk(p, isArray));
		if(isArray){
			::operator delete[](p);
		}else{
			::operator delete(p);
		}
	}
};
 
static void* gc_obj::operator new(size_t size){
	return gc::instance().alloc(size);
}
 
static void* gc_obj::operator new[](size_t size){
	return gc::instance().alloc(size, true);
}
 
static void  gc_obj::operator delete(void* p) {
	gc::instance().free(p);
}
 
static void  gc_obj::operator delete[](void* p) {
	gc::instance().free(p, true);
}
 
gc::~gc(){
	list<chunk>::iterator it;
	list<chunk> leak_list(mem_list);
	int i;
	cout<<"===============memory leak list==============\n";
	for(i=1, it=leak_list.begin(); it!=leak_list.end(); ++it, ++i){
		cout<<i<<": obj["<<hex<<it->ptr<<dec
			<<"]\tfinal release...";
	if(it->isArray)
		delete[] (gc_obj*)((char*)it->ptr+4);  //this is a trick, C++ add 4 bytes to record demension.
	else
		delete ((gc_obj*)it->ptr);
	}
}
 
int main(int argc, char* argv[]){
	class Foo: public gc_obj{
	public:
		Foo(string name):gc_obj(name){}
		Foo():gc_obj("noname_Foo"){};
	};
 
	Foo* p1 = new Foo("obj1");
	Foo* p2 = new Foo("obj leak");
	Foo* p3 = new Foo[5];
	delete p1;
	p2 =0;  //mem leak
	p3 =0;  //array leak
 
	return 0;
}
