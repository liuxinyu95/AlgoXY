#ifndef __REFLECT__
#define __REFLECT__

#include <string>
#include <list>
#include <map>
#include <algorithm>
#include "testcase.h"
#include "testscript.h"
#include "testcaller.h"

class Class;

class Object{
public:
	Object(){}
	virtual ~Object(){}

	const Class* getClass() const { return itsClass_; }
	void setClass(const Class* aClass){ itsClass_=aClass; }

	const std::string toString() const { return name_; }
	void setName(const std::string& name){
		name_=name;
	}

private:
	const Class* itsClass_;
	std::string name_;
};

class Constructor{
public:
	virtual Object* operator()()=0;
};

template<class T>
struct Ctor: public Constructor{
	T* operator()(){ return new T; }
};

class Method{
	virtual Object* operator()(std::list<Object*> params)=0;
};

class Class{
public:
	Class(std::string name):className_(name), ctor_(0){}
	Class(const Class& x):className_(x.className_), ctor_(x.ctor_){}
	Class& operator=(Class& x){
		className_ = x.className_;
		ctor_ = x.ctor_;
		return *this;
	};

	~Class(){
		delete ctor_;
	}

	Object* newInstance(){
		Object* ptr;
		if(ctor_)
			ptr = (*ctor_)();
		else
			ptr= new Object;
		ptr->setClass(this);
		return ptr;
	}

	static Class& forName(const std::string& name);

	const std::string getName() const { return className_; }

	const Constructor* getConstructor() const { return ctor_; }
	void setConstructor(Constructor* ctor){
		ctor_ = ctor;
	}

private:
	std::string className_;
	Constructor* ctor_;	//should use auto_ptr???
};

class ClassManager{
public:
	static ClassManager& instance(){
		static ClassManager inst;
		return inst;
	}

	void add(Class* x){
		coll_[x->getName()]=x;
	}

	Class& get(const std::string& name){
		return *coll_[name];
	}

	//only for debug
	void clear(){
		coll_.clear();
	}

	~ClassManager(){
		std::map<std::string, Class*>::iterator pos;
		for(pos=coll_.begin(); pos!=coll_.end(); ++pos)
			delete pos->second;
	}

private:
	std::map<std::string, Class*> coll_;
};

Class& Class::forName(const std::string& name){
	return ClassManager::instance().get(name);
}

#define DECLARE_REFLECTIVE(x)	\
static bool registerClass(){	\
	Class* inst = new Class(#x);	\
	inst->setConstructor(new Ctor<x>);	\
	ClassManager::instance().add(inst);	\
	return true;	\
}

#define IMPLEMENT_REFLECTIVE(x)	\
namespace{	\
	const bool reflect_res=x::registerClass();	\
}

class Lion: public Object{
public:
	Lion(){}
	void hunt(){
		std::cout<<"hunting....\n";
	}

	DECLARE_REFLECTIVE(Lion)
};

class TestReflection : public TestCase
{
	void test(){
		TestScript<TestReflection>::begin(this)
			<<&TestReflection::testZoo
			<<&TestReflection::testAddClass
			<<&TestReflection::testDynaConstruct
			<<end;
	}

	void testZoo(){
		std::cout<<"============Reflection test ============\n";
		Class Lion("Lion");
		Object* kidLion = Lion.newInstance();
		std::cout<<kidLion->getClass()->getName()<<"\n";
		kidLion->setName("Simba");
		std::cout<<kidLion->toString()<<"\n";
		delete kidLion;
	}

	void printInfo(Object* p){
		std::cout<<"object: "<<p->toString()<<" is an instance of class: "
			<<p->getClass()->getName()<<"\n";
	}

	void testAddClass(){
		ClassManager::instance().add(new Class("Lion"));
		ClassManager::instance().add(new Class("Monkey"));

		Object* mufasa = Class::forName("Lion").newInstance();
		mufasa->setName("Mufasa");
		Object* monkeyKing = Class::forName("Monkey").newInstance();
		monkeyKing->setName("Sun WuKong");

		printInfo(mufasa);
		printInfo(monkeyKing);

		ClassManager::instance().add(new Class("Panda"));
		Object* panpan = Class::forName("Panda").newInstance();
		panpan->setName("Pan-pan");
		printInfo(panpan);

		delete mufasa;
		delete monkeyKing;
		delete panpan;
	}

	void testDynaConstruct(){
		ClassManager::instance().clear();
		const bool res=Lion::registerClass();

		Lion* simba = dynamic_cast<Lion*>(Class::forName("Lion").newInstance());
		simba->hunt();
		delete simba;
	}
};
#endif
