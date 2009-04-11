#include <iostream>
#include <vector>
#include <algorithm>
//#include "foo.h"

using namespace std;

typedef string Date;

//
// some POD definitions:
//
struct Employee{
	int		Number;
	string	Name;
	enum	JobType{ CONTRACT, PART_TIME, STANDARD};
	JobType	Type;
};

//
//
//
struct Mailbox{
	short Id;
	string User;
	int	  Size;
};

struct CreditCard{
	string		SerialNumber;
	string		Owner;
	Date		ExpireDate;
};

//
// Resouce with ID holder Policy
// 1. provide ID type
// 2. provide Data type
// 3. provide get_id() method
//
struct EmployeeHolder: public Employee{
	typedef int ID;
	typedef Employee Data;
	
	EmployeeHolder(){}
	EmployeeHolder(const Data& v){Number=v.Number; Name=v.Name; Type=v.Type;}
	EmployeeHolder& operator=(const Data& v){
		Number=v.Number; Name=v.Name; Type=v.Type;
		return *this;
	}
	
	const ID get_id() const { return Number; }
};

struct MailboxHolder: public Mailbox{
	typedef short ID;
	typedef Mailbox Data;
	
	MailboxHolder(){}
	MailboxHolder(const Data& v){Id=v.Id; User=v.User; Size=v.Size;}
	MailboxHolder& operator=(const Data& v){
		Id=v.Id; User=v.User; Size=v.Size;
		return *this;
	}
	
	const ID get_id() const { return Id; }
};

struct CreditCardHolder: public CreditCard{
	typedef long long	ID;
	typedef CreditCard	Data;
	
	CreditCardHolder(){}
	CreditCardHolder(const Data& v){
		SerialNumber=v.SerialNumber; Owner=v.Owner; ExpireDate=v.ExpireDate;
	}
	CreditCardHolder& operator=(const Data& v){
		SerialNumber=v.SerialNumber; Owner=v.Owner; ExpireDate=v.ExpireDate;
		return *this;
	}
	
	const ID get_id() const {
		ID res=0;
		string::const_iterator it=SerialNumber.begin();
		for(int j=0; j<4; ++j){
			for(int i=0; i<4; ++i, ++it)
				res = res*10+(*it)-'0';
			if(it!=SerialNumber.end())
				++it;	//skip the "-" char
		}
		return res;
	}
};

//
// Resouce Operation Policy
// 1. provide eq()
// 2. provide lt()
// 3. provide diff()
//
template<class T>
struct NormalOp{
	bool eq(const T& v1, const T& v2) const {return v1.get_id() == v2.get_id(); }
	bool lt(const T& v1, const T& v2) const {return v1.get_id() < v2.get_id(); }
	typename T::ID diff(const T& v1, const T& v2) const {return v1.get_id() - v2.get_id(); }
};

template<class T>
struct ReverseOp: public NormalOp<T>{
	bool lt(const T& v1, const T& v2) const {return v1.get_id() > v2.get_id(); }
};

template<class T>
struct CreditOp: public NormalOp<T>{
	typename T::ID diff(const T& v1, const T& v2) const {
		return (v1.get_id() - v2.get_id())/10000; 
	}
};

template<class DataHolder, template<class> class Op = NormalOp>
class Resource: public DataHolder, public Op<DataHolder>{
private:
	typedef Resource<DataHolder, Op>	_Self;
	typedef typename DataHolder::Data	_Data;
public:
	Resource(){}
	Resource(const _Data& v):DataHolder(v){}
	_Self& operator=(const _Data& v){ 
		DataHolder::operator=(v);
		return *this;
	}

	const bool operator==(const _Self& v) const { return eq(*this, v); }
	const bool operator< (const _Self& v) const { return lt(*this, v); }
	typename DataHolder::ID operator- (const _Self& v){ return diff(*this, v); }

	//to get data, just use up-cast to <Data>
};

class ExamRoom{
public:
	template<class T>
	static int test(vector<T> coll){
		// 既然支持比较运算符，可以直接用STL了
		sort(coll.begin(), coll.end());
		for(vector<T>::iterator it=coll.begin(); it!=coll.end(); ++it)
			cout<<it->get_id()<<" ";

		// 计算能够预留出来的位置
		int tables=0;
		for(unsigned int i=0; i<coll.size()-1; ++i)
			if(abs(coll[i+1]-coll[i])>1)
				++tables;
		cout<<"\n"<<tables<<"\n";
		return tables;
	}
};

class MyException:public runtime_error{
public:
	MyException(const string& whatString):runtime_error(whatString){
	}
};

void f(){
	throw MyException("my prob");
}

class B_interface{
public:
	virtual void operate_bar(int b)=0;
	virtual ~B_interface(){};
};

namespace ns1{
	class B;
}

class A{
public:
	A(B_interface* ptr):p(ptr){}
	void func(){
		cout<<"foo\n";
		p->operate_bar(3);
	}
private:
	B_interface* p;
};

namespace ns1{
	class B:public B_interface{
	public:
		void proc();
		void operate_bar(int b){
			bar=b;
		}
	private:
		int bar;
	};

	void B::proc(){
		A* ptr=new A(this);
		ptr->func();
		delete ptr;
	}
}

int main(int argc, char* argv[]){
	typedef Resource<EmployeeHolder> EmployeeResource;
	
	const Employee person[3]={{3, "Tom", Employee::CONTRACT},
							  {1, "Jack", Employee::PART_TIME}, 
							  {4, "Mike", Employee::STANDARD}};

	vector<EmployeeResource> coll;
	for(int i=0; i<3; ++i)
		coll.push_back(person[i]);
	ExamRoom::test(coll);

	typedef Resource<MailboxHolder, ReverseOp> MailboxResource;
	const Mailbox box[3]={{2, "Inbox", 128},
						  {1, "Outbox", 32}, 
						  {5, "Draft", 5}};

	vector<MailboxResource> coll2;
	for(int i=0; i<3; ++i)
		coll2.push_back(box[i]);
	ExamRoom::test(coll2);

	typedef Resource<CreditCardHolder, CreditOp> CreditCardResource;
	const CreditCard card[3]={{"0514-2048-3016-3210", "Tom", "08/05"},
						  {"0514-2048-3016-1024", "Jack", "06/07"}, 
						  {"0514-2048-3012-4231", "Mike", "06/12"}};

	vector<CreditCardResource> coll3;
	for(int i=0; i<3; ++i)
		coll3.push_back(card[i]);
	ExamRoom::test(coll3);

	try{
		f();
	}
	catch(MyException& e){
		cout<<e.what()<<endl;
	}

	ns1::B b;
	b.proc();
}

//end of file
