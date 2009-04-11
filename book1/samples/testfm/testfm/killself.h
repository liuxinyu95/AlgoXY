#ifndef __KILL_SELF__
#define __KILL_SELF__

class KillSelf{
public:
	static KillSelf& create(){
		KillSelf* self=new KillSelf;
		return *self;
	}

	template<class T>
	KillSelf& operator<<(T x){
		std::cout<<x;
		return *this;
	}

	typedef KillSelf& (*_Manipulator)(KillSelf&);
	KillSelf& operator<<(_Manipulator op){
		return op(*this);
	}

	void suicide(){ delete this; }

	~KillSelf(){
		std::cout<<"killed\n";
	}
};

inline KillSelf& kill(KillSelf& self){
	self.suicide();
	return self;
}

struct TestKillSelf{
	static void test(){
		delete &(KillSelf::create()<<"kill self "<<5<<"...\n");
		KillSelf::create()<<"kill self "<<5<<"...\n"<<kill;
	}
};

#endif // __KILL_SELF__
