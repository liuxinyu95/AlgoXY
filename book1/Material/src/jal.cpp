#include "jal.h"
#include "jal_common.h"
#include "jal_sys.h"
#include "jal_login.h"
#include "jal_mailbox.h"
//#include "jal_document.h"
#include <iostream>

using namespace std;

int is_sys_ok(){
	return jal::is_sys_ok()? 1:0;
}

void* login(const char* user, const char* passwd) {
	try{
		jal::jal_login* ptr_login=new jal::jal_login(std::string(user), std::string(passwd));
		return ptr_login;
	}
	catch(runtime_error& e){
		cerr<<DBG_MARK<<e.what();
	}
	return NULL;
}

void logout(void* ptr_login){
	try{
		if(ptr_login)
			delete static_cast<jal::jal_login*>(ptr_login);
	}
	catch(runtime_error& e){
		cerr<<DBG_MARK<<e.what();
	}
}

int get_mailbox(void* ptr_login, void** result) {
	try {
		if(!ptr_login)
			throw runtime_error("ptr_login is NULL.");
			
		jal::jal_mailbox mailbox(static_cast<jal::jal_login*>(ptr_login));
		set<jal::jal_mailbox::item_attrib> coll=mailbox.get_mailbox();
		set<jal::jal_mailbox::item_attrib>::iterator it;
		int i;
		
		cout<<"list mailbox..."<<endl;
		struct mailbox_item_attrib* boxes = (struct mailbox_item_attrib*)malloc(coll.size()*sizeof(struct mailbox_item_attrib));
		for(i=0, it = coll.begin(); it != coll.end(); ++it, ++i) {
			cout<<"["<<i<<"]:"
				<<"box name = "<<it->get_name()<<"\t"
				<<"box id = "<<it->get_id()<<endl;
			boxes[i].box_id = it->get_id();
			boxes[i].name   = strdup(it->get_name().c_str());
		}
		cout<<"finish."<<endl;
		
		(*result)=boxes;
		return i;
	}
	catch(runtime_error& e) {
		cerr<<DBG_MARK<<e.what();
	}
	return -1;
}

int add_mailbox(void* ptr_login, const char* box_name, const char* box_passwd) {
	try {
		if(!ptr_login)
			throw runtime_error("ptr_login is NULL.");
			
		jal::jal_mailbox mailbox(static_cast<jal::jal_login*>(ptr_login));
		mailbox.add_mailbox(string(box_name), string(box_passwd));
		return 0;
	}
	catch(runtime_error& e) {
		cerr<<DBG_MARK<<e.what();
	}
	return -1;
}

extern int add_mailbox_with_id(
							void* ptr_login, 
							short box_id,
							const char* box_name, 
							const char* box_passwd) 
{
	try {
		if(!ptr_login)
			throw runtime_error("ptr_login is NULL.");
			
		jal::jal_mailbox mailbox(static_cast<jal::jal_login*>(ptr_login));
		mailbox.add_mailbox(box_id, string(box_name), string(box_passwd));
		return 0;
	}
	catch(runtime_error& e) {
		cerr<<DBG_MARK<<e.what();
	}
	return -1;
}

int del_mailbox(void* ptr_login, short box_id) {
	try {
		if(!ptr_login)
			throw runtime_error("ptr_login is NULL.");
			
		jal::jal_mailbox mailbox(static_cast<jal::jal_login*>(ptr_login));
		mailbox.del_mailbox(box_id);
		return 0;
	}
	catch(runtime_error& e) {
		cerr<<DBG_MARK<<e.what();
	}
	return -1;
}

int get_document(short box_id, void* ptr_login, void** result) {
	/*
	try {
		if(!ptr_login)
			throw runtime_error("ptr_login is NULL.");
			
		jal::jal_document document(static_cast<jal::jal_login*>(ptr_login));
		set<jal::jal_document::item_attrib> coll=document.get_document(box_id);
		set<jal::jal_document::item_attrib>::iterator it;

		int i;
//		cout<<"list document..."<<endl;
		struct document_item_attrib* docs = (struct document_item_attrib*)malloc(coll.size()*sizeof(struct document_item_attrib));
		for(i=0, it = coll.begin(); it != coll.end(); ++it, ++i) {
//			cout<<"["<<i<<"]:"
//				<<"doc name = "<<it->get_name()<<"\t"
//				<<"doc id = "<<it->get_box_file_id()<<endl;
			docs[i].box_file_id = it->get_box_file_id();
			docs[i].name   = strdup(it->get_name().c_str());
//            cout<<"["<<i<<"]:"
//              <<"doc name = "<<docs[i].name<<"\t"
//              <<"doc id = "<<docs[i].box_file_id<<endl;
		}
//		cout<<"jal::get_document() finish."<<endl;
		
		(*result)=docs;
		return i;
	}
	catch(runtime_error& e) {
		cerr<<DBG_MARK<<e.what();
	}
	*/
	return -1;
}

int get_doc_attributes(void* ptr_login, long long doc_id, const char* box_passwd, void** result) {
	/*
    try {
        if(!ptr_login)
            throw runtime_error("ptr_login is NULL.");
        
        cout<<"doc_id = "<<doc_id<<endl;
        jal::jal_document document(static_cast<jal::jal_login*>(ptr_login));
        set<jal::jal_document::attr_key_value> coll = document.get_doc_attributes(doc_id, box_passwd);
        set<jal::jal_document::attr_key_value>::iterator it;
        
        int i;
        cout<<"list attribute..."<<endl;
        struct attr_key_value* attrs = (struct attr_key_value*)malloc(coll.size()*sizeof(struct attr_key_value));
        for(i=0, it = coll.begin(); it != coll.end(); ++it, ++i) {
            attrs[i].key = strdup(it->get_key().c_str());
            attrs[i].value = strdup(it->get_value().c_str());
        }
        cout<<"finish."<<endl;
        
        (*result) = attrs;
        return i;
    }
    catch(runtime_error& e) {
        cerr<<DBG_MARK<<e.what();
    }
    */
    return -1;
            
}

int del_document(void* ptr_login, long long doc_id, short box_id, const char* box_passwd) {
    bool res = false;
    /*
    try {
        if(!ptr_login)
            throw runtime_error("ptr_login is NULL.");
        
        cout<<"doc_id = "<<doc_id<<endl;
        jal::jal_document document(static_cast<jal::jal_login*>(ptr_login));
        res = document.del_document(doc_id, box_id, string(box_passwd));
    }
    catch(runtime_error& e) {
        cerr<<DBG_MARK<<e.what();
    }
    */
    if (res) {
        return 1;
    } else {
        return 0;
    }
        
}

//end
