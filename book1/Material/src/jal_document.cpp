#include "jcl_document.h"
#include <iostream>
#include <algorithm>

using namespace std;
using namespace jp::co::fujixerox::spider::util::mailbox;
using namespace jp::co::fujixerox::spider::util::commons;
using namespace jp::co::fujixerox::spider::jail::proxylib;

namespace jcl{

/**
 * get MailboxUtility Interface by jail proxy lib
 */
void jcl_document::init() throw(runtime_error) {
	if(!jcl::is_sys_ok())
		throw runtime_error("system not OK.");
			
	CORBA::Object_var obj = CJailProxyLib::getInterface(MailboxUtility::NAME);
	if(CORBA::is_nil(obj))
		throw runtime_error("can not get mailbox utility.");
	
	_util = MailboxUtility::_narrow(obj);

    // get BoxIF
	_ops  = _util->getBoxIF();
	
	if(CORBA::is_nil(_ops))
		throw runtime_error("can not get box interface");
}

/**
 * get document list of mailbox
 * @param box_id short mailbox id
 * @return _cache_set document list 
 */
std::set<jcl_document::item_attrib> 
jcl_document::get_document(short box_id) throw(runtime_error) {
    cerr<<DBG_MARK<<"enter "<<__FUNCTION__<<endl;
    
    if(!_plogin) {
    	cerr<<DBG_MARK<<"user hasn't been logged in"<<endl;
    	throw runtime_error("need login first");
    }

    CommonsDef::StringSeq names;
    names.length(2);
    names[0] = CORBA::string_dup(MailboxDef::NAME); // document name
    names[1] = CORBA::string_dup(MailboxDef::BOX_FILE_ID);  // document id

    const char* pwd = " ";  // password, it's needless in iterator_1
    MailboxDef::FilterConditionSeq  filters; 
	MailboxDef::SortConditionSeq    sorts;
    CORBA::Long 					start_position = 0;
    CORBA::Long 					getting_volume = 21;
//    CORBA::Short					doc_id = maildoc_id;
    MailboxDef::MailboxElementAttributeSeq *documentList = 0;
    
    // get document list from mailbox
    BoxIF::BoxOperationResult_var result = _ops->getList( 
        _plogin->get_ticket(),
        box_id,
		pwd,                	//const char* pwd, 
        names,
        filters,
        sorts,
        start_position,
        getting_volume,
        documentList);
    
    if( result->returnStatus != BoxIF::SUCCESS ) {
        cerr<<DBG_MARK
        	<<"UTIL#getList() err= " << result->returnStatus <<endl;
    }
    
    // insert document into result set
    std::set<item_attrib> _cache_set;
    CORBA::Long length = documentList->length();
    cerr<<DBG_MARK<< "get_document() length="<<length<<endl;
    for( CORBA::Long i=0; i<length; i++ ) {
        _cache_set.insert((*documentList)[i]);
    }

    delete documentList;

	return _cache_set;
}

/**
 * get document attributes by doc_id and box password.
 * only attributes as below can be got:
 *      NAME
 *      BOX_FILE_ID
 *      TYPE
 *      CREATED_DATE
 *      HIDDEN
 * @param doc_id long long document ID
 * @param box_passwd std::string password of mailbox
 * @return attributes list
 */
std::set<jcl_document::attr_key_value> jcl_document::get_doc_attributes(long long doc_id, std::string box_passwd) throw(std::runtime_error) {
	cerr<<DBG_MARK<<"enter "<<__FUNCTION__<<endl;
    
    if(!_plogin) {
    	cerr<<DBG_MARK<<"user hasn't been logged in"<<endl;
    	throw runtime_error("need login first");
    }
    
    // get BoxFileWriterIF interface
//   	BoxFileWriterIF_var _fwr = _util->getBoxFileWriterIF();
    BoxFileReaderIF_var _fwr = _util->getBoxFileReaderIF();	
	
    if(CORBA::is_nil(_fwr)) {
		throw runtime_error("can not get filewriter interface");
	}
    
    // get BoxFileIF interface
	BoxFileIF_var _bfi = _util->getBoxFileIF();
    
	if(CORBA::is_nil(_bfi)) {
		throw runtime_error("can not get boxfile interface");
	}
	
	/*MailboxDef::BoxFileWriterID box_file_w_id;
    // create boxFileWriter, it's necessary to get document attributes
    BoxFileIF::BoxFileOperationResult_var res = _bfi->createBoxFileWriter(
    	_plogin->get_ticket(),
    	doc_id,
    	box_passwd.c_str(),		// password
    	box_file_w_id);
    
    if(res->returnStatus != BoxFileIF::SUCCESS){
    	cerr<<DBG_MARK
        	<<"UTIL#createBoxFileWriter() err= " << res->returnStatus <<endl;
        throw runtime_error("createBoxFileWriter interface error ");
    }*/
    
    MailboxDef::BoxFileReaderID box_file_r_id;
    // create boxFileWriter, it's necessary to get document attributes
    BoxFileIF::BoxFileOperationResult_var res = _bfi->createBoxFileReader(
        _plogin->get_ticket(),
        doc_id,
        box_passwd.c_str(),     // password
        box_file_r_id);
    
    if(res->returnStatus != BoxFileIF::SUCCESS){
        cerr<<DBG_MARK
            <<"UTIL#createBoxFileWriter() err= " << res->returnStatus <<endl;
        throw runtime_error("createBoxFileWriter interface error ");
    }
    
    
//    cerr<<DBG_MARK<<"boxFileWriterID=0x"<<hex<<box_file_w_id<<dec<<endl;
    
    // now, we only can get flowing attributes:
    // NAME, BOX_FILE_ID, TYPE, CREATED_DATE, HIDDEN
	CommonsDef::StringSeq names;
	names.length(5);
	names[0] = CORBA::string_dup(MailboxDef::NAME);
	names[1] = CORBA::string_dup(MailboxDef::TYPE);
    names[2] = CORBA::string_dup(MailboxDef::CREATED_DATE);
	names[3] = CORBA::string_dup(MailboxDef::BOX_FILE_ID);
    names[4] = CORBA::string_dup(MailboxDef::HIDDEN);
    
    CommonsDef::KeyValueSeq_var attributes;
    
    /*BoxFileWriterIF::BoxFileWriteOperationResult_var result = _fwr->getAttributes(
    		_plogin->get_ticket(),
    		box_file_w_id,
    		names,
    		attributes);
    
    if( result->returnStatus != BoxFileWriterIF::SUCCESS ) {
        cerr<<DBG_MARK
        	<<"UTIL#getAttributes() err= " << result->returnStatus <<endl;
        _bfi->releaseBoxFileWriter(_plogin->get_ticket(), box_file_w_id);
    	throw runtime_error("createBoxFileWriter interface error ");
    }
    CORBA::Long length = attributes->length();
//    cerr<<"========== Attr List Begin ==============="<<endl;
//    for( CORBA::ULong i=0; i<length; i++ ) {
//    	cerr<<attributes[i].key<<" : "<<attributes[i].value<<endl;
//    }
//    cerr<<"========== Attr List End ==============="<<endl;

    // release boxFileWriter, this step is necessary.
	_bfi->releaseBoxFileWriter(_plogin->get_ticket(), box_file_w_id);
*/    
    BoxFileReaderIF::BoxFileReadOperationResult_var result = _fwr->getAttributes(
            _plogin->get_ticket(),
            box_file_r_id,
            names,
            attributes);
    
    if( result->returnStatus != BoxFileReaderIF::SUCCESS ) {
        cerr<<DBG_MARK
            <<"UTIL#getAttributes() err= " << result->returnStatus <<endl;
        _bfi->releaseBoxFileReader(_plogin->get_ticket(), box_file_r_id);
        throw runtime_error("createBoxFileWriter interface error ");
    }
    CORBA::Long length = attributes->length();
//    cerr<<"========== Attr List Begin ==============="<<endl;
//    for( CORBA::ULong i=0; i<length; i++ ) {
//      cerr<<attributes[i].key<<" : "<<attributes[i].value<<endl;
//    }
//    cerr<<"========== Attr List End ==============="<<endl;

    // release boxFileWriter, this step is necessary.
//    _bfi->releaseBoxFileWriter(_plogin->get_ticket(), box_file_w_id);
    _bfi->releaseBoxFileReader(_plogin->get_ticket(), box_file_r_id);
        
    std::set<attr_key_value> attr_set;

    for( CORBA::Long i=0; i<length; i++ ) {
        attr_set.insert((*attributes)[i]);
    }
    
//    delete attributes;

    return attr_set;
}

/**
 * delete document by doc_id
 */
int jcl_document::del_document(long long doc_id, short box_id, std::string box_passwd) throw(std::runtime_error) {
    cerr<<DBG_MARK<<"enter "<<__FUNCTION__<<endl;
    
    if(!_plogin) {
        cerr<<DBG_MARK<<"user hasn't been logged in"<<endl;
        throw runtime_error("need login first");
    }
    
    // get BoxWriterIF interface
    BoxWriterIF_var _bwi = _util->getBoxWriterIF();
    if(CORBA::is_nil(_bwi)) {
        throw runtime_error("can not get boxWriterIF interface");
    }
    
    // create boxWriter (Don't forget to release it...)
    MailboxDef::BoxWriterID boxWriterID;
    BoxIF::BoxOperationResult_var res1 = _ops->createBoxWriter(
            _plogin->get_ticket(),
            box_id,
            box_passwd.c_str(),
            boxWriterID);
            
    if (res1->returnStatus != BoxIF::SUCCESS) {
        cerr<<" failed to delete doc, res1: "<<res1->returnStatus<<endl;   
        cerr<<" error: "<<res1->detail<<endl;   
        return 0;
    }

    // delete document by doc_id
    cerr<< "doc_id to be deleted is : "<<doc_id<<endl;
    
    BoxWriterIF::BoxWriteOperationResult_var res2 = _bwi->disposeBoxFile(
            _plogin->get_ticket(),
            boxWriterID,
            doc_id);
    
    // release boxWriter
    res1 = _util->getBoxIF()->releaseBoxWriter(
            _plogin->get_ticket(),
            boxWriterID);
    
    if (res2->returnStatus != BoxWriterIF::SUCCESS
        || res1->returnStatus != BoxIF::SUCCESS) {
        cerr<<" failed to delete doc, res1: "<<res1->returnStatus<<" | res2: "<<res2->returnStatus<<endl;
        return 0;
    }
    cerr<<"finished."<<endl;
    
    return 1;
}

/*int jcl_document::get_attribute_names(long long doc_id, std::string box_passwd) throw(std::runtime_error) {
    cerr<<DBG_MARK<<"enter "<<__FUNCTION__<<endl;
    if(!_plogin) {
        cerr<<DBG_MARK<<"user hasn't been logged in"<<endl;
        throw runtime_error("need login first");
    }
    cerr<<DBG_MARK<<"doc tobe deleted is : "<<doc_id<<endl;
    // get BoxFileWriterIF interface
    BoxFileWriterIF_var _fwr = _util->getBoxFileWriterIF();
    
    if(CORBA::is_nil(_fwr)) {
        throw runtime_error("can not get filewriter interface");
    }
    
    // get BoxFileIF interface
    BoxFileIF_var _bfi = _util->getBoxFileIF();
    
    if(CORBA::is_nil(_bfi)) {
        throw runtime_error("can not get boxfile interface");
    }
    
    MailboxDef::BoxFileWriterID box_file_w_id;
    // create boxFileWriter, it's necessary to get document attributes
    BoxFileIF::BoxFileOperationResult_var res = _bfi->createBoxFileWriter(
        _plogin->get_ticket(),
        doc_id,
        box_passwd.c_str(),     // password
        box_file_w_id);
    
    if(res->returnStatus != BoxFileIF::SUCCESS){
        cerr<<DBG_MARK
            <<"UTIL#createBoxFileWriter() err= " << res->returnStatus <<endl;
        throw runtime_error("createBoxFileWriter interface error ");
    }
    
//    cerr<<DBG_MARK<<"boxFileWriterID=0x"<<hex<<box_file_w_id<<dec<<endl;
    
    // now, we only can get flowing attributes:
    // NAME, BOX_FILE_ID, TYPE, CREATED_DATE, HIDDEN
    CommonsDef::StringSeq_var names;
    
    BoxFileWriterIF::BoxFileWriteOperationResult_var result = _fwr->getAttributeNames(
            _plogin->get_ticket(),
            box_file_w_id,
            names);
    
    if( result->returnStatus != BoxFileWriterIF::SUCCESS ) {
        cerr<<DBG_MARK
            <<"UTIL#getAttributeNames() err= " << result->returnStatus <<endl;
        cerr<<DBG_MARK
            <<"UTIL#errmsg: " << result->detail <<endl;
        _bfi->releaseBoxFileWriter(_plogin->get_ticket(), box_file_w_id);
        throw runtime_error("createBoxFileWriter interface error ");
    }
    CORBA::Long length = names->length();
//    cerr<<"========== Attr List Begin ==============="<<endl;
//    for( CORBA::ULong i=0; i<length; i++ ) {
//      cerr<<attributes[i].key<<" : "<<attributes[i].value<<endl;
//    }
//    cerr<<"========== Attr List End ==============="<<endl;

    // release boxFileWriter, this step is necessary.
    _bfi->releaseBoxFileWriter(_plogin->get_ticket(), box_file_w_id);
    
    cerr<<DBG_MARK<<"=========== output attribute names =============="<<endl;
    for( CORBA::Long i=0; i<length; i++ ) {
        const char* name = (*names)[i];
        cerr<<DBG_MARK<<name<<endl;
    }
    cerr<<DBG_MARK<<"================================================="<<endl;
    return length;
    
}*/

class is_adjacent{
public:
	template<typename T>
	bool operator()(const T& elem, const T& next_elem) {
		if(next_elem - elem >1)
			return true;
		return false;
	}
};

int jcl_document::get_empty_doc_id(short box_id){
	set<jcl_document::item_attrib> coll = get_document(box_id);
	if(coll.empty())
		return 1;
		
	set<jcl_document::item_attrib>::iterator it = coll.begin();
	if(it->get_id() > 1)
		return 1;
		
	is_adjacent op;
	it=adjacent_find(coll.begin(), coll.end(), op);	
	if(it == coll.end())
		--it;		//return 0;
	
	return it->get_id()+1 > 21 ? 0 : it->get_id()+1;
}


}//end ns jcl
//end
