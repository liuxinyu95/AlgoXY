#ifndef _JAL_DOCUMENT_H_
#define _JAL_DOCUMENT_H_

#include "jal_common.h"
#include "jal_login.h"

#include <set>
#include <string>
#include <stdexcept>

namespace jal{
	
class jal_document {
public:

	template<class T> 
	struct doc_order_ops{
		typedef long long S;
		bool eq(const T& v1, const T& v2) const { return v1.boxFileID == v2.boxFileID; }
		
		bool lt(const T& v1, const T& v2) const { return v1.boxFileID < v2.boxFileID; }
		
		S diff(const T& v1, const T& v2) const { return v1.boxFileID - v2.boxFileID; }
		
		//S get_key(const T& v) const { return v.boxFileID; }
	};
	
	typedef jal_item_attrib<jp::co::fujixerox::spider::util::mailbox::MailboxDef::MailboxElementAttribute, doc_order_ops> item_attrib;
	
	const static short MAX_BOX_ID	= 21;
    
public:
	class attr_key_value {
	public:
		attr_key_value(){}
		
		attr_key_value(const attr_key_value& rhv):
			key(rhv.key), value(rhv.value){}
			
		attr_key_value(const jp::co::fujixerox::spider::util::commons::CommonsDef::KeyValue &rhv):
			key(rhv.key), value(rhv.value){}
		
		attr_key_value& operator=(const attr_key_value& rhv) {
			key = rhv.key;
			value = rhv.value;
			return *this;
		}
		
		attr_key_value& operator=(const jp::co::fujixerox::spider::util::commons::CommonsDef::KeyValue &rhv) {
			key = rhv.key;
			value = rhv.value;
			return *this;
		}
        
        bool operator == (const attr_key_value& v) {
            if (key == v.key && value == v.value) {
                return true;
            } else {
                return false;
            }
        }
        
/*        int operator - (attr_key_value v) {
            return key - v.key;
        }
        
        int operator - (const attr_key_value& v) const {
            return _box_file_id - v._box_file_id;
        }
        */
        bool operator < (const attr_key_value& v) const {
            return (key < v.key);
        } 
        
        bool operator < (attr_key_value v) {
            return (key < v.key);
        } 
		
		const std::string get_key() const {
			return key;
		}
		
		const std::string get_value() const {
			return value;
		}
		
	public:
		std::string key;
		std::string value;
	};

public:

	//
	//ctor & copy ctor
	//
	
	jal_document(): _plogin(NULL) {
		init();
	}
	
	jal_document(jal_login* ptr_login) : _plogin(ptr_login) {
		init();
	}
	
	jal_document(jal_document& rhv):
		_util(rhv._util), _ops(rhv._ops), _plogin(rhv._plogin){}
	
	void init() throw(std::runtime_error);
	
	void add_document(short box_id, std::string box_password, std::string doc_name) throw(std::runtime_error);
	std::set<item_attrib> get_document(short box_id) throw(std::runtime_error);
	std::set<attr_key_value> get_doc_attributes(long long doc_id, std::string box_passwd) throw(std::runtime_error);
//    bool del_document(long long doc_id, short box_id, std::string box_passwd) throw(std::runtime_error);
    int del_document(long long doc_id, short box_id, std::string box_passwd) throw(std::runtime_error);
//    int jal_document::get_attribute_names(long long doc_id, std::string box_passwd) throw(std::runtime_error);
//	std::set<attribute_key_value> get_attribute(item_attrib attr) throw(std::runtime_error);
private:
	int get_empty_doc_id(short box_id);

private:
	jp::co::fujixerox::spider::util::mailbox::MailboxUtility_var _util;
	jp::co::fujixerox::spider::util::mailbox::BoxIF_var			_ops;
	
	jal_login*					_plogin;
//	std::set<item_attrib>		_cache_set;
};

}//end ns jal

#endif //_JAL_DOCUMENT_H_

