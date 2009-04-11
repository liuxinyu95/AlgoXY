#ifndef _JCL_H_
#define _JCL_H_

#ifdef __cplusplus
extern "C"
{
#endif

//
// system basic
//

extern int is_sys_ok();

//
// login facilities
//

extern void* login(const char* user, const char* passwd);

extern void logout(void* ptr_login);

//
// mailbox stuffs
//

struct mailbox_item_attrib {
	int 		box_type;
	short 		box_id;
	long long	box_file_id;
	const char*	name;
};

//
// return the number of mailbox got. -1 if meet error.
//

extern int get_mailbox(void* ptr_login, void** result);

//
// return 0 if OK, a negative number if meet error.
//

extern int add_mailbox(void* ptr_login, const char* box_name, const char* box_passwd);

extern int add_mailbox_with_id(
					void* ptr_login, 
					short box_id,
					const char* box_name, 
					const char* box_passwd);

extern int del_mailbox(void* ptr_login, short box_id);

//
// document stuffs
//

struct document_item_attrib {
	int 		box_type;
	short 		box_id;
	long long	box_file_id;
	const char*	name;
};

//
// return the number of document got. -1 if meet error.
//

extern int get_document(short box_id, void* ptr_login, void** result);

/*
 * attribute key-value struct
 */
struct attr_key_value {
    const char* key;
    const char* value;
};

/*
 * return the number of attribute got. -1 if meet error.
 */
extern int get_doc_attributes(void* ptr_login, long long doc_id, const char* box_passwd, void** result);

/**
 * return 1 if delete successfully, else return 0.
 */
extern int del_document(void* ptr_login, long long doc_id, short box_id, const char* box_passwd);

#ifdef __cplusplus
}
#endif

#endif //_JCL_H_
