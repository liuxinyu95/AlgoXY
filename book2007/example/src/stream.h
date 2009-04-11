#ifndef __STREAM__
#define __STREAM__

class record{
public:
	record(double v, record* r=0): value(v), _next(r){}
	~record(){ delete _next; }

	template<class Func, class Arg>
	record* setNext(Func f, record* r, Arg arg){
		_next=f(r->next(), arg);
		return this;
	}

	virtual record* next(){ return _next;}

	double value;

private:
	record* _next;
};

template<class T=double> class stream_record;

template<class Func, class Obj, class Arg, class T>
stream_record<T>* create_delay_record(Func f, Obj x, Arg arg);

template<class T>
class stream_record{
public:
	stream_record():value(0), _next(0){}
	stream_record(T v, stream_record* r=0):value(v), _next(r){}
	virtual ~stream_record(){ 
		delete _next;
	}

	template<class Func, class Obj, class Arg>
	stream_record* setNext(Func f, Obj x, Arg arg){
		_next = create_delay_record<Func, Obj, Arg, T>(f, x, arg);
		return this;
	}

	stream_record* setNext(stream_record* r){
		_next = r;
		return this;
	}

	virtual stream_record* operator()(){ return this;}

	stream_record* next(){
		if(_next)
			_next=(*_next)();
		return _next;
	}
	T value;

private:
	stream_record* _next;
};

template<class Func, class Obj, class Arg, class T>
class delayed_record: public stream_record<T>{
public:
	delayed_record(Func f, Obj x, Arg arg): _f(f), _x(x), _arg(arg){}
	stream_record<T>* operator()(){
		std::cout<<"\nlazy eval: ";
		stream_record<T>* res = _f(_arg(_x), _arg);
		delete this;
		return res;
	}

private:
	Func _f;
	Obj  _x;
	Arg  _arg;
};

//partial instatiate
template<class Func, class Arg, class T>
class delayed_record<Func, stream_record<T>*, Arg, T>: public stream_record<T>{
public:
	delayed_record(Func f, stream_record<T>* x, Arg arg): _f(f), _x(x), _arg(arg){}
	stream_record<T>* operator()(){
		std::cout<<"\nstill empty, eval: ";
		stream_record<T>* res = _f(_x->next(), _arg);
		delete this;
		return res;
	}

private:
	Func _f;
	stream_record<T>* _x;
	Arg  _arg;
};

//parial instatiate
template<class Func, class T>
class delayed_record<
	Func, 
	stream_record<T>*, stream_record<T>*, 
	T>: public stream_record<T>{
public:
	delayed_record(Func f, stream_record<T>* x, stream_record<T>* y): _f(f), _x(x), _y(y){}
	stream_record<T>* operator()(){
		stream_record<T>* res = _f(_x->next(), _y->next());
		delete this;
		return res;
	}

private:
	Func _f;
	stream_record<T>* _x;
	stream_record<T>* _y;
};

template<class Func, class Obj, class Arg, class T>
stream_record<T>* create_delay_record(Func f, Obj x, Arg arg){
	return new delayed_record<Func, Obj, Arg, T>(f, x, arg);
}

template<typename Record, typename F>
Record* transform(Record* r, F f){
	if(r){
		return (new Record(f(r->value)))->setNext(transform<Record, F>, r, f);
	}
	return 0;
}

template<typename Record, typename F>
Record* filter(Record* r, F f){
	if(r)
		if(f(r->value))
			return (new Record(r->value))->setNext(filter<Record, F>, r, f);
		else
			return filter(r->next(), f);
	return 0;
}

template<class T>
struct AddTo{
	AddTo(int n):_n(n){}
	T operator()(T x){ return x+_n; }
	T _n;
};

template<typename Succ>
stream_record<int>* create_series(int n, Succ f){
	return (new stream_record<int>(n))->setNext(create_series<Succ>, n, f);
}

template<typename Record>
double getAt(Record* r, int i){
	if(i==0)
		return (r!=0)? r->value : -1;
	else
		return getAt(r->next(), i-1);
}

double discount(double x){
	if(x>=300)
		x*=0.7;
	else if(x>=200.00)
		x*=0.85;
	else if(x>=100.0)
		x*=0.9;
	return x;
}

template<typename Record>
void print_rec(Record* r){ 
	if(r){
		std::cout<<r->value<<", "; 
		print_rec(r->next());
	}
	else
		std::cout<<"\n";
}

template<typename T>
struct Greator{
	Greator(double v):x(v){};
	bool operator()(T y){return y>x; }
	double x;
};

template<class Record>
Record* add_series(Record* a, Record* b){
	return (new Record(a->value+b->value))->setNext(add_series<Record>, a, b);
}

//float result=getAt(filter(transform(r, discount()), Greator(500), 100);

#endif
