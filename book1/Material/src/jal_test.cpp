//
// jal headers
//

#include "jal_common.h"
#include "jal_sys.h"
#include "jal_login.h"
#include "jal_mailbox.h"
//#include "jal_document.h"


//
// STL headers
//
#include <string>
#include <set>

//
// cpp unit headers
//

#include <cppunit/TestCase.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestResult.h>	

#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

/*
using namespace jp::co::fujixerox::spider::runtime::user;
using namespace jp::co::fujixerox::spider::afw;
using namespace jp::co::fujixerox::spider::util::commons;
using namespace jp::co::fujixerox::spider::util::mailbox;
using namespace jp::co::fujixerox::spider::jail::proxylib;
*/

using namespace std;
using namespace CppUnit;

class jal_console: public CppUnit::TestFixture {
public:
	jal_console() {}
	
	void test_menu() {
		try {
			jal::jal_login login("fxadmin", "fxadmin");
			jal::jal_mailbox mailbox(&login);
			//jal::jal_document document(&login);
			
			while(true) {
				cout<<"================== jal console ================"<<endl
					<<"1. display mailbox & documents"<<endl
					<<"2. add a mailbox"<<endl
					<<"3. del a mailbox"<<endl
					<<"4. set a mailbox"<<endl
					<<"5. exit"<<endl
					<<"==============================================="<<endl
					<<"[enter a cmd:]==>";
					
				int cmd;
				cin >> cmd;
				list_mailbox(&mailbox);
				switch(cmd) {
				case 1:
					//test_doc_menu(&mailbox, &document);
					break;
				case 2:
					test_add_mailbox(&mailbox);
					break;
				case 3:
					test_del_mailbox(&mailbox);
					break;
				case 4:
					test_set_mailbox(&mailbox);
					break;
				default:
					return;
				}
				list_mailbox(&mailbox);
			}
		} catch (runtime_error& e) {
			cerr<<"e.what()"<<endl;
		}
	}

private:
	
	/*
	void test_doc_menu(jal::jal_mailbox* ptr_mailbox, jal::jal_document* ptr_doc) {
		while(true) {
			cout<<"[select a mailbox (-1 to exit, 0 for all):]==>"<<endl;
			short box_id;
			cin >> box_id;
			if(box_id<0)
				return;
				
			cout<<"================== document menu==============="<<endl
				<<"1. display documents & attribs"<<endl
				<<"2. del a document"<<endl
				<<"3. exit"<<endl
				<<"==============================================="<<endl
				<<"[enter a cmd:]==>";
					
			int cmd;
			cin >> cmd;
			list_documents(ptr_doc, box_id);
			switch(cmd) {
			case 1:
				test_get_attributes(ptr_doc, ptr_mailbox, box_id);
    		break;
			case 2:
				if(box_id == 0)
					cout<<"Not support del all document in every mailbox"<<endl;
				else
					test_delete_document(ptr_doc, box_id);
				break;
			default:
				return;
			}
			list_documents(ptr_doc, box_id);
		}
	}
	*/
	
	void test_del_mailbox(jal::jal_mailbox* ptr_mailbox) {
		cout<<"================ del_mailbox test==================="<<endl
			<<"select a box_id (-1 to exit)==>";
			
		short  box_id;
		cin >> box_id;
		if(box_id > 0){
			ptr_mailbox->del_mailbox(box_id);
		}
	}
	
	void test_add_mailbox(jal::jal_mailbox* ptr_mailbox) {
		cout<<"================ add_mailbox test==================="<<endl
			<<"\t\t1. Automatic select id"<<endl
			<<"\t\t2. Given an id"<<endl
			<<"\t\t3. Skip this test"<<endl
			<<"select[1/2/3]==>";
				
		short select;
		string box_name;
		string box_passwd;
		short  box_id;
			
		cin >> select;
		if(select == 3)
			return;
				
		cout<< "enter box name==>";
		cin >> box_name;
		cout<< "enter box passwd(>8 chars)==>";
		cin >> box_passwd;	
			
		switch(select){
		case 1:
			ptr_mailbox->add_mailbox(box_name, box_passwd);
			break;
		case 2:
			cout<< "enter id(1-21)==>";
			cin >> box_id;
			ptr_mailbox->add_mailbox(box_id, box_name, box_passwd);
			break;
		default:
			;
		}
	}
	
	void test_set_mailbox(jal::jal_mailbox* ptr_mailbox) {
		cout<<"================ set_mailbox test==================="<<endl;
			
		short  box_id;
		cout<< "enter id(1-21, -1 to skip)==>";
		cin >> box_id;
		if(box_id != -1) {
			string new_name;
			cout<<"enter the new box name ==>";
			cin >> new_name;
			ptr_mailbox->set_mailbox(box_id, new_name);
		}
	}
	
	/*
	void test_get_attributes(jal::jal_document* ptr_document, jal::jal_mailbox* ptr_mailbox, short box_id) {
		if(box_id == 0) {
			set<jal::jal_mailbox::item_attrib> coll=ptr_mailbox->get_mailbox();
			set<jal::jal_mailbox::item_attrib>::iterator it;
			int i;
			for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
				box_id = it->get_id();
				cout<<"["<<i<<"]:"
					<<"box name = "<<it->get_name()<<"\t"
					<<"box id = "<<it->get_id()<<endl;
				list_doc_attributes(ptr_document, box_id);
//                list_attribute_names(ptr_document, box_id);
			}
		}
		else
			list_doc_attributes(ptr_document, box_id);
//        list_attribute_names(ptr_document, box_id);
	}
	*/
	
	/*
	void list_doc_attributes(jal::jal_document* ptr_document, short box_id) {
		set<jal::jal_document::item_attrib> coll_b=ptr_document->get_document(box_id);
		set<jal::jal_document::item_attrib>::iterator it_b;

		int j;
		for(j=1, it_b = coll_b.begin(); it_b != coll_b.end(); ++it_b, ++j) {
			cout<<"  ["<<j<<"]:\n"
          	        <<"\tbox id = "<<it_b->get_id()<<" : "
				<<" |doc name = "<<it_b->get_name()<<" : "
				<<" | doc id = "<<it_b->get_box_file_id()<<endl;
                        
            // begin get document attributes
            set<jal::jal_document::attr_key_value> coll_c = ptr_document->get_doc_attributes(it_b->get_box_file_id(), " ");
            set<jal::jal_document::attr_key_value>::iterator it_c;
                    
            int k;
   	        for(k=1, it_c = coll_c.begin(); it_c != coll_c.end(); ++it_c, ++k) {
       	        cout<<"\t"<<it_c->get_key()<<" | "<<it_c->get_value()<<endl;
           	}

		}
	}
	*/
    
    /*
    void test_delete_document(jal::jal_document* ptr_document, short box_id) {
        std::string box_passwd = " ";
        long long doc_id;
            
        cout<<"[enter doc_id(-1 to skip)]==>"<<endl;
        cin>>doc_id;
        if (doc_id != -1) {
            bool res = ptr_document->del_document(doc_id, box_id, box_passwd);
            if (res) {
                cout << "delete success!"<<endl;
            } else {
                cout << "delete failed!"<<endl;
            }
        }
    }
    */
	
 	void list_mailbox(jal::jal_mailbox* ptr_mailbox) {
		set<jal::jal_mailbox::item_attrib> coll=ptr_mailbox->get_mailbox();
		set<jal::jal_mailbox::item_attrib>::iterator it;
		int i;
		cout<<"list mailbox..."<<endl;
		for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
			cout<<"["<<i<<"]:"
				<<"box name = "<<it->get_name()<<"\t"
				<<"box id = "<<it->get_id()<<endl;
		}
		cout<<"finish."<<endl;
	}
    
    /*
    void list_documents(jal::jal_document* ptr_document, short box_id) {
        set<jal::jal_document::item_attrib> coll=ptr_document->get_document(box_id);
        set<jal::jal_document::item_attrib>::iterator it;
        int i;
        cout<<"list document..."<<endl;
        for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
            cout<<"["<<i<<"]:"
                <<"name = "<<it->get_name()<<"\t"
                <<"id = "<<it->get_box_file_id()<<endl;
        }
        cout<<"finish."<<endl;
    }
    */
    
    CPPUNIT_TEST_SUITE(jal_console);
	CPPUNIT_TEST(test_menu);
    CPPUNIT_TEST_SUITE_END();
};

CPPUNIT_REGISTRY_ADD("ui", "/");
CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(jal_console, "ui" );

class jal_test: public CppUnit::TestFixture {
public:
	jal_test() {
		//_system_ok = jal::is_sys_ok();
	}
	
	void setUp(){}
	
	void tearDown(){}
	
	void test_login_as_admin(){
		try{
			jal::jal_login login("fxadmin", "fxadmin");
		}catch(runtime_error& e){
			cerr<<e.what()<<endl;
		}
	}
	
	void test_login_as_guest(){
		try{
			jal::jal_login("guest", "guest");
		}catch(runtime_error& e){
			cerr<<e.what()<<endl;;
		}
	}
	
	void test_login_wrong_pwd() {
		jal::jal_login("fxadmin","12345");
	}
	
	void test_login_nobody() {
		jal::jal_login("nobody","");
	}
	
	void test_get_mailbox() {
		try{
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			list_mailbox(&mailbox);
		}
		catch(runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	void test_add_mailboxes_ok(){
		try{
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			del_all_mailboxes(&mailbox);
			
			mailbox.add_mailbox("mb01", "123456789");
			mailbox.add_mailbox("mb02", "123456789");
			mailbox.add_mailbox(5, "mb05", "123456789");
			mailbox.add_mailbox("mb03", "123456789");
			
			string names[4]={"mb01", "mb02", "mb03", "mb05"};
			short  ids[4] = {1, 2, 3, 5};
			assert_mailboxes(&mailbox, names, ids, 4);
		} catch (runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	void test_add_limit_mailbox() {
		try {
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			del_all_mailboxes(&mailbox);
			
			mailbox.add_mailbox(1, "mb01", "123456789");
			mailbox.add_mailbox(21,"mb21", "123456789");
			
			string names[4]={"mb01", "mb21"};
			short  ids[4] = {1, 21};
			assert_mailboxes(&mailbox, names, ids, 2);
		} catch (runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	void test_add_mb0() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
		
		mailbox.add_mailbox(0, "mb0", "123456789");
	}
	
	void test_add_mb_minus_id() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
		
		mailbox.add_mailbox(-1, "mb-1", "123456789");
	}
	
	void test_add_mb_exceed() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
		
		mailbox.add_mailbox(22, "mb22", "123456789");
	}
	
	void test_del_mailbox_ok() {
		try {
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			del_all_mailboxes(&mailbox);
			
			mailbox.add_mailbox("mb01", "123456789");
			mailbox.add_mailbox("mb02", "123456789");
			mailbox.add_mailbox(5, "mb05", "123456789");
			mailbox.add_mailbox("mb03", "123456789");
			
			mailbox.del_mailbox(2);
			mailbox.del_mailbox(3);
			
			string names[4]={"mb01", "mb05"};
			short  ids[4] = {1, 5};
			assert_mailboxes(&mailbox, names, ids, 2);
		}catch(runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	void test_del_non_mb() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
		
		mailbox.add_mailbox("mb01", "123456789");
		mailbox.add_mailbox("mb02", "123456789");
		mailbox.add_mailbox(5, "mb05", "123456789");
		mailbox.add_mailbox("mb03", "123456789");
		
		mailbox.del_mailbox(10);
	}
	
	void test_del_mb_twice() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
		
		mailbox.add_mailbox("mb01", "123456789");
		mailbox.add_mailbox("mb02", "123456789");
		mailbox.add_mailbox(5, "mb05", "123456789");
		mailbox.add_mailbox("mb03", "123456789");
		
		mailbox.del_mailbox(1);
		mailbox.del_mailbox(1);
	}
	
	void test_set_mailbox_ok() {
		try{
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			del_all_mailboxes(&mailbox);
			
			mailbox.add_mailbox("mb01", "123456789");
			mailbox.add_mailbox("mb02", "123456789");
			mailbox.add_mailbox(5, "mb05", "123456789");
			mailbox.add_mailbox("mb03", "123456789");
			
			mailbox.set_mailbox(1, "mb-1st");
			mailbox.set_mailbox(3, "mb-3rd");
			
			string names[4]={"mb-1st", "mb02", "mb-3rd", "mb05"};
			short  ids[4] = {1, 2, 3, 5};
			assert_mailboxes(&mailbox, names, ids, 4);
		}
		catch(runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	void test_set_non_mb() {
		jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
		jal::jal_mailbox mailbox(&login);
		del_all_mailboxes(&mailbox);
			
		mailbox.add_mailbox("mb01", "123456789");
		mailbox.add_mailbox("mb02", "123456789");
		mailbox.add_mailbox(5, "mb05", "123456789");
		mailbox.add_mailbox("mb03", "123456789");
		
		mailbox.set_mailbox(7, "mb07");
	}
	
	void test_set_mb_twice() {
		try{
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			jal::jal_mailbox mailbox(&login);
			del_all_mailboxes(&mailbox);
			
			mailbox.add_mailbox("mb01", "123456789");
			mailbox.add_mailbox("mb02", "123456789");
			mailbox.add_mailbox(5, "mb05", "123456789");
			mailbox.add_mailbox("mb03", "123456789");
			
			mailbox.set_mailbox(1, "mb-1st");
			mailbox.set_mailbox(1, "mb-first");
			
			string names[4]={"mb-first", "mb02", "mb03", "mb05"};
			short  ids[4] = {1, 2, 3, 5};
			assert_mailboxes(&mailbox, names, ids, 4);
		}
		catch(runtime_error& e) {
			cerr<<e.what()<<endl;
		}
	}
	
	/*
	void test_get_document() {
		try{
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			
			jal::jal_mailbox mailbox(&login);
			set<jal::jal_mailbox::item_attrib> coll=mailbox.get_mailbox();
			set<jal::jal_mailbox::item_attrib>::iterator it;
			int i;
			for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
				short box_id = it->get_id();
				// begin get document
				jal::jal_document document(&login);
				set<jal::jal_document::item_attrib> coll_b=document.get_document(box_id);
				set<jal::jal_document::item_attrib>::iterator it_b;

				cout<<"["<<i<<"]:"
					<<"box name = "<<it->get_name()<<"\t"
					<<"box id = "<<it->get_id()<<endl;
				int j;
				for(j=1, it_b = coll_b.begin(); it_b != coll_b.end(); ++it_b, ++j) {
					cout<<"  ["<<j<<"]:\n"
						<<"\t  box id = "<<it_b->get_id()<<"\n"
						<<"\t  doc name = "<<it_b->get_name()<<"\n"
						<<"\t  doc id = "<<it_b->get_box_file_id()<<endl;
				}
				// end get document
			}
			cout<<"finish."<<endl;
		}
		catch(runtime_error& e) {
			cerr<<e.what();
		}
	}
	*/
	
	void test_get_attributes() {
		try{
//			long long doc_id = 2;
			jal::jal_login login("fxadmin", "fxadmin");		//larry: auto logout
			
			jal::jal_mailbox mailbox(&login);
			set<jal::jal_mailbox::item_attrib> coll=mailbox.get_mailbox();
			set<jal::jal_mailbox::item_attrib>::iterator it;
			int i;
			for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
				short box_id = it->get_id();
				// begin get document
				/*
				jal::jal_document document(&login);
				
				set<jal::jal_document::item_attrib> coll_b=document.get_document(box_id);
				set<jal::jal_document::item_attrib>::iterator it_b;

				cout<<"["<<i<<"]:"
					<<"box name = "<<it->get_name()<<" | "
					<<"box id = "<<it->get_id()<<endl;
				int j;
				for(j=1, it_b = coll_b.begin(); it_b != coll_b.end(); ++it_b, ++j) {
					cout<<"  ["<<j<<"]:\n"
                        <<"\tbox id = "<<it_b->get_id()<<" : "
						<<" |doc name = "<<it_b->get_name()<<" : "
						<<" | doc id = "<<it_b->get_box_file_id()<<endl;
                        
                    // begin get document attributes
                    set<jal::jal_document::attr_key_value> coll_c = document.get_doc_attributes(it_b->get_box_file_id(), " ");
                    set<jal::jal_document::attr_key_value>::iterator it_c;
                    
                    int k;
                    for(k=1, it_c = coll_c.begin(); it_c != coll_c.end(); ++it_c, ++k) {
                        cout<<"\t"<<it_c->get_key()<<" | "<<it_c->get_value()<<endl;
                    }
					
				}
				*/
				// end get document
			}
			cout<<"finish."<<endl;
		}
		catch(runtime_error& e) {
			cerr<<e.what();
		}
	}
    
    /*
    void test_delete_document() {
        try{
            jal::jal_login login("fxadmin", "fxadmin");
            jal::jal_document document(&login);
            
            list_documents(&document);
            
            short box_id = 1;
            std::string box_passwd = " ";
            long long doc_id;
            
            cout<<"enter doc_id(-1 to skip)"<<endl;
            cin>>doc_id;
            if (doc_id != -1) {
                bool res = document.del_document(doc_id, box_id, box_passwd);
                if (res) {
                    cout << "delete success!"<<endl;
                } else {
                    cout << "delete failed!"<<endl;
                }
                list_documents(&document);
            }
        } catch(runtime_error& e) {
            cerr<<e.what();
        }
    }
    */
	
	CPPUNIT_TEST_SUITE(jal_test);
	CPPUNIT_TEST(test_login_as_admin);
	CPPUNIT_TEST(test_login_as_guest);
	CPPUNIT_TEST_EXCEPTION(test_login_wrong_pwd, runtime_error);
	CPPUNIT_TEST_EXCEPTION(test_login_nobody, runtime_error);
	CPPUNIT_TEST(test_get_mailbox);
	CPPUNIT_TEST(test_add_mailboxes_ok);
	CPPUNIT_TEST(test_add_limit_mailbox);
	CPPUNIT_TEST_EXCEPTION(test_add_mb0, runtime_error);
	CPPUNIT_TEST_EXCEPTION(test_add_mb_minus_id, runtime_error);
	//CPPUNIT_TEST_EXCEPTION(test_add_mb22, runtime_error);
	CPPUNIT_TEST(test_del_mailbox_ok);
	CPPUNIT_TEST_EXCEPTION(test_del_non_mb, runtime_error);
	CPPUNIT_TEST_EXCEPTION(test_del_mb_twice, runtime_error);
	CPPUNIT_TEST(test_set_mailbox_ok);
	CPPUNIT_TEST_EXCEPTION(test_set_non_mb, runtime_error);
	CPPUNIT_TEST(test_set_mb_twice);
	//CPPUNIT_TEST(test_get_document);
    //CPPUNIT_TEST(test_get_attributes);	//JAL
    //CPPUNIT_TEST(test_delete_document);
    
	//CPPUNIT_TEST_EXCEPTION(test_login, runtime_error);	
	CPPUNIT_TEST_SUITE_END();

private:

	void list_mailbox(jal::jal_mailbox* ptr_mailbox) {
		try{
			set<jal::jal_mailbox::item_attrib> coll=ptr_mailbox->get_mailbox();
			set<jal::jal_mailbox::item_attrib>::iterator it;
			int i;
			cout<<"list mailbox..."<<endl;
			for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
				cout<<"["<<i<<"]:"
					<<"box name = "<<it->get_name()<<"\t"
					<<"box id = "<<it->get_id()<<endl;
			}
			cout<<"finish."<<endl;
		}
		catch(runtime_error& e) {
			cerr<<e.what();
		}
	}
    
    /*
    void list_documents(jal::jal_document* ptr_document) {
        try{
            set<jal::jal_document::item_attrib> coll=ptr_document->get_document(1);
            set<jal::jal_document::item_attrib>::iterator it;
            int i;
            cout<<"list document..."<<endl;
            for(i=1, it = coll.begin(); it != coll.end(); ++it, ++i) {
                cout<<"["<<i<<"]:"
                    <<"name = "<<it->get_name()<<"\t"
                    <<"id = "<<it->get_box_file_id()<<endl;
            }
            cout<<"finish."<<endl;
        }
        catch(runtime_error& e) {
            cerr<<e.what();
        }
    }
    */
    
    int del_all_mailboxes(jal::jal_mailbox* pmb){
    	set<jal::jal_mailbox::item_attrib> coll=pmb->get_mailbox();
        set<jal::jal_mailbox::item_attrib>::iterator it;
        for(it = coll.begin(); it != coll.end(); ++it) {
        	cout<<"box: "<<it->get_name()<<"\t"
                <<"id = "<<it->get_id()
                <<"delete.\n";
            pmb->del_mailbox(it->get_id());
        }
        return coll.size();
    }
    
    void assert_mailboxes(jal::jal_mailbox* pmb, string* names, short* ids, int n) {
    	set<jal::jal_mailbox::item_attrib> coll=pmb->get_mailbox();
    	CPPUNIT_ASSERT( n == coll.size() );
        set<jal::jal_mailbox::item_attrib>::iterator it;
        int i=0;
        for(it = coll.begin(); it != coll.end(); ++it, ++i) {
        	cout<<"["<<i+1<<"]:"
                    <<"name = "<<it->get_name()<<"\t"
                    <<"id = "<<it->get_id()<<endl;
        	CPPUNIT_ASSERT( it->get_name() == names[i] && it->get_id() == ids[i]);
        }
    }
};

CPPUNIT_REGISTRY_ADD("silent", "/");
CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(jal_test, "silent" );

int main(int argc, char* argv[]){
	
	string test_path = argc>1 ? string(argv[1]) : "";
	
	TextUi::TestRunner runner;
	TestFactoryRegistry &registry = TestFactoryRegistry::getRegistry("/");
	runner.addTest(registry.makeTest());
	
	try{
		bool isOK=runner.run(test_path, false);
		return isOK ? 0: 1;
	}
	catch (std::invalid_argument& e) {
		cerr<<e.what()<<endl;
	}
	return 0;
}

//end
