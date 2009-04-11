#ifndef _JAL_MAILBOX_H_
#define _JAL_MAILBOX_H_

#include "jal_common.h"
#include "jal_login.h"
#include <set>
#include <string>
#include <stdexcept>

namespace jal{
const static short MAX_BOX_ID	= 21;

struct mailbox{
	short boxID;
	std::string name;
};

struct MailboxHolder: public mailbox{
	typedef short ID;
	typedef mailbox Data;
	
	MailboxHolder(){};
	MailboxHolder(const mailbox& v) { boxID = v.boxID; name  = v.name; }
	MailboxHolder& operator = (const mailbox& v) {
		boxID = v.boxID; name  = v.name;
		return *this;
	}
	
	const ID get_id() const { return boxID; }
	
	//
	// the following function is just compatible with Iteration-1
	//
	
	const std::string& get_name() const { return name; }
};

class jal_mailbox {
public:
	typedef resource<MailboxHolder> item_attrib;
	
	
	jal_mailbox(jal_login* ptr_login) : _plogin(ptr_login) { 
		if(!_plogin) throw std::runtime_error("please login 1st.");
		init();
	}
	
	jal_mailbox(const jal_mailbox& v):	_plogin(v._plogin){}
		
	jal_mailbox& operator = (const jal_mailbox& v){
		_plogin=v._plogin;
		return *this;
	}
	
	void init() throw(std::runtime_error);
	
	void add_mailbox(std::string box_name, std::string box_passwd) throw(std::runtime_error);
	void add_mailbox(short box_id, std::string box_name, std::string box_passwd) throw(std::runtime_error);
	std::set<item_attrib> get_mailbox() throw(std::runtime_error);
	void del_mailbox(short box_id) throw(std::runtime_error);
	void set_mailbox(short box_id, std::string new_name) throw(std::runtime_error);
	void set_mailbox(short box_id, std::pair<std::string, std::string> attrib) throw(std::runtime_error);
	
private:
	jal_mailbox(){}
	int get_empty_box_id();
	
	jal_login*					_plogin;
};

class box_stub{
public:
	static box_stub& instance(){
		static box_stub inst;
		return inst;
	}
	
	char* box_coll[MAX_BOX_ID];
	
	bool add(short id, const char* name){
		if(id<=0 || id >= MAX_BOX_ID) return false;
		if(box_coll[id]) return false;
		
		box_coll[id] = strdup(name);
		return true;
	}
	
	bool del(short id){
		if(id<=0 || id >= MAX_BOX_ID) return false;
		if(box_coll[id] == NULL) return false;
		
		free(box_coll[id]);
		box_coll[id] = NULL;
		return true;
	}
	
	bool set(short id, const char* newname){
		if(id<=0 || id >= MAX_BOX_ID) return false;
		if(box_coll[id] == NULL) return false;
		
		free(box_coll[id]);
		box_coll[id] = strdup(newname);
		return true;
	}
	
	std::set<jal_mailbox::item_attrib> get(){
		std::set<jal_mailbox::item_attrib> coll;
		struct mailbox box;
		for(int i=1; i<MAX_BOX_ID; ++i){
			if(box_coll[i]){
				box.boxID = i;
				box.name = std::string(box_coll[i]);
				coll.insert(box);
			}
		}
		return coll;
	}
	
private:
	box_stub(){
		for(int i=0; i<MAX_BOX_ID; ++i)
			box_coll[i]=NULL;
	};
	
	box_stub(const box_stub&);
	box_stub& operator = (const box_stub&);
	
	~box_stub(){
		for(int i=0; i<MAX_BOX_ID; ++i){
			if(box_coll[i]) 
				free(box_coll[i]);
		}
	};
};

}//end ns jal
#endif //_JAL_MAILBOX_H_
