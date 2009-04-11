#include <string>
#include <set>

//
// RAW data structs
//
//--------------------------------------------------------------------------
struct mailbox{
	short id;
	std::string name;
};

struct doc{
	long fileID;
	std::string owner;
};

struct job{
	int jobID;
	enum jobtype{ scan, print, copy };
	jobtype type;
};

//
// Data holder policy instance
//
//-------------------------------------------------------------------------
struct MailboxHolder: public mailbox{
	typedef short ID;
	typedef mailbox Data;
	
	MailboxHolder(){}
	MailboxHolder(const mailbox& v){id=v.id; name=v.name;}
	MailboxHolder& operator=(const mailbox& v){
		id=v.id; name = v.name; 
		return *this;
	}
	
	const ID get_id() const { return id; }

	void disp(){
		std::cout<<"id="<<id<<" name="<<name<<"\t";
	}
};

struct JobHolder: public job{
	typedef int ID;
	typedef job Data;
	
	JobHolder(){}
	JobHolder(const job& v){jobID=v.jobID; type=v.type;}
	JobHolder& operator=(const job& v){
		jobID=v.jobID; type = v.type; 
		return *this;
	}
	
	const ID get_id() const { return jobID; }

	void disp(){
		const static std::string type_msg[3]={"scan", "print", "copy"};
		std::cout<<"id="<<jobID<<" msg="<<type_msg[type]<<"\t";
	}
};

struct DocHolder: public doc{
	typedef long ID;
	typedef doc  Data;
	
	DocHolder(){}
	DocHolder(const doc& v){fileID=v.fileID; owner=v.owner;}
	DocHolder& operator=(const doc& v){
		fileID=v.fileID; owner = v.owner; 
		return *this;
	}
	
	const ID get_id() const { return fileID; }

	void disp(){
		std::cout<<"id="<<fileID<<" msg="<<owner<<"\t";
	}
};

//
// Operation policy instance
//
//-------------------------------------------------------------------------
template<class T>
struct IndexOp{
	bool eq(const T& v1, const T& v2) const {return v1.get_id() == v2.get_id(); }
	bool lt(const T& v1, const T& v2) const {return v1.get_id() > v2.get_id(); }
	typename T::ID diff(const T& v1, const T& v2) const {return v1.get_id() - v2.get_id(); }
};

//--------------------------------------------------------------------------

template<class DataHolder, template<class> class Op = IndexOp>
class idres: public DataHolder, public Op<DataHolder>{
private:
	typedef idres<DataHolder, Op>		_self;
	typedef typename DataHolder::Data	_Data;
public:
	idres(){}
	idres(const _Data& v):DataHolder(v){}
	_self& operator=(const _Data& v){ 
		DataHolder::operator=(v);
		return *this;
	}

	const bool operator==(const _self& v) const { return eq(*this, v); }
	const bool operator< (const _self& v) const { return lt(*this, v); }
	typename DataHolder::ID operator- (const _self& v){ return diff(*this, v); }

	//to get data, just use up-cast to <Data>
};


template<class Resource>
struct ByName{
	ByName(std::string str){
		target= str;
	}

	typedef Resource Elem;
	bool is_match(Elem a){
		return a.name == target;
	}
private:
	std::string target;
};

template<class ByWho>
std::set<typename ByWho::Elem> getBy(std::set<typename ByWho::Elem> coll, ByWho who){
	std::set<typename ByWho::Elem> res;
	std::set<typename ByWho::Elem>::iterator it;
	//
	// remove_copy()
	//
	for(it = coll.begin(); it != coll.end(); ++it){
		if(who.is_match(*it))
			res.insert(*it);
	}
	return res;
}

void test_idres(){
	mailbox mb[3]	={ {1, "box1"}, {2, "box-2"}, {5, "mybox"}};
	doc		docs[3]	={ {1002, "title1"}, {2012, "section-001"}, {5050, "chapter-end"}};
	job		jobs[3]	={ {10, job::scan}, {71, job::print}, {90, job::copy}};
	typedef std::set<idres<MailboxHolder> >	MailSet;
	typedef std::set<idres<DocHolder> >		DocSet;
	typedef std::set<idres<JobHolder> >		JobSet;

	MailSet mb_coll;
	DocSet	doc_coll;
	JobSet	job_coll;

	for(int i=0; i<3; ++i){
		mb_coll.insert(mb[i]);
		doc_coll.insert(docs[i]);
		job_coll.insert(jobs[i]);
	}

	MailSet::iterator	mb_it;
	DocSet::iterator	doc_it;
	JobSet::iterator	job_it;
	for(mb_it =mb_coll.begin(), doc_it =doc_coll.begin(),	job_it =job_coll.begin();
		mb_it!=mb_coll.end(),	doc_it!=doc_coll.end(),		job_it!=job_coll.end();
		++mb_it,				++doc_it,					++job_it){
			mb_it->disp(); doc_it->disp(); job_it->disp();
			std::cout<<"\n";
	}

	ByName<idres<MailboxHolder> > byName("box-2");
	MailSet res=getBy(mb_coll, byName);
	std::cout<<"get OK\n";
}