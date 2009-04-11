#include "jal_mailbox.h"
#include <iostream>
#include <algorithm>

using namespace std;

namespace jal{

void jal_mailbox::init() throw(runtime_error) {
	//
	//perform low-level init for mailbox util
	//
}

void 
jal_mailbox::add_mailbox(
							short box_id, 
							string box_name, 
							string box_passwd) throw(runtime_error)
{
	struct mailbox newbox;
	newbox.boxID = box_id;
	newbox.name = box_name;
	
	box_stub::instance().add(box_id, box_name.c_str());
    cerr<<DBG_MARK<<"create mailbox OK"<<endl;
}							

void 
jal_mailbox::add_mailbox(string box_name, 
							  string box_passwd) throw(runtime_error)
{
	//
	// find the empty box_id
	//
	int box_id=get_empty_box_id();
	cerr<<DBG_MARK<<"empty box_id = "<<box_id<<endl;
    if(!box_id){
    	throw runtime_error("no empty id to add a mailbox.");
    }
    
    add_mailbox(box_id, box_name, box_passwd);
}

void 
jal_mailbox::del_mailbox(short box_id) throw(runtime_error)
{
	box_stub::instance().del(box_id);
    cerr<<DBG_MARK<<"delete mailbox OK"<<endl;
}

void jal_mailbox::set_mailbox(short box_id, string new_name) throw(runtime_error) 
{
	box_stub::instance().set(box_id, new_name.c_str());
}

void 
jal_mailbox::set_mailbox(
						short box_id, 
						pair<string, string> attrib) throw(runtime_error)
{
	//not implement
}

set<jal_mailbox::item_attrib> 
jal_mailbox::get_mailbox() throw(runtime_error) {
	return box_stub::instance().get();
}

class is_adjacent{
public:
	template<typename T>
	bool operator()(const T& elem, const T& next_elem) {
		if(next_elem - elem >1)
			return true;
		return false;
	}
};

int jal_mailbox::get_empty_box_id(){
	set<jal_mailbox::item_attrib> coll = get_mailbox();
	if(coll.empty())
		return 1;
		
	set<jal_mailbox::item_attrib>::iterator it = coll.begin();
	if(it->get_id() > 1)
		return 1;
		
	is_adjacent op;
	it=adjacent_find(coll.begin(), coll.end(), op);	
	if(it == coll.end())
		--it;		//return 0;
	
	return it->get_id()+1 > MAX_BOX_ID ? 0 : it->get_id()+1;
}

}//end ns jal
//end
