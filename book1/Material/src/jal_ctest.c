#include <stdio.h>

#ifdef __USE_DLL__
//--------------------------- using dll method --------------------------
#include <dlfcn.h>
#include <unistd.h>

int   (*is_sys_ok)();
void* (*login)(const char* user, const char* passwd);
void  (*logout)(void* ptr_login);
int	  (*get_mailbox)(void* ptr_login, void** result);
int   (*add_mailbox)(void* ptr_login, const char* box_name, const char* box_passwd);
int	  (*get_document)(short box_id, void* ptr_login, void** result);
int   (*get_doc_attributes)(void* ptr_login, long long doc_id, const char* box_passwd, void** result);
int   (*del_document)(void* ptr_login, long long doc_id, short box_id, const char* box_passwd);

struct mailbox_item_attrib {
	int 		box_type;
	short 		box_id;
	long long	box_file_id;
	const char*	name;
};

struct document_item_attrib {
	int 		box_type;
	short 		box_id;
	long long	box_file_id;
	const char*	name;
};

struct attr_key_value {
    const char* key;
    const char* value;
};

void* get_api() {
	void* handle = NULL;
	const char* dl_error;
	char  lib_filename[255];
	char* ptr;
	
	is_sys_ok 	= NULL;
	login 		= NULL;
	logout 		= NULL;
	get_mailbox = NULL;
	add_mailbox = NULL;
	get_document = NULL;
	del_document = NULL;
	
	getcwd(lib_filename, 255);
	if((ptr=strstr(lib_filename,"/bin")) != NULL) {
		*ptr = '\0';
	}
	strcat(lib_filename, "/lib/libjal.so");
	handle = dlopen(lib_filename, RTLD_LAZY);
	dl_error = dlerror();
    if (dl_error || handle==NULL) {
		printf("can not open %s!\n", lib_filename);
		return NULL;
    } else {
		printf("libjal.so opend.\n");
    }
    
	is_sys_ok = dlsym(handle, "is_sys_ok");
		
	dl_error = dlerror();
	if (dl_error || is_sys_ok == NULL) {
    	printf("can not get is_sys_ok()\n");
		return NULL;
    } else {
    	printf("got is_sys_ok()\n");
    }
    
    login = dlsym(handle, "login");
		
	dl_error = dlerror();
	if (dl_error || login == NULL) {
    	printf("can not get login()\n");
		return NULL;
    } else {
    	printf("got login()\n");
    }
    
    logout = dlsym(handle, "logout");
		
	dl_error = dlerror();
	if (dl_error || logout == NULL) {
    	printf("can not get logout()\n");
		return NULL;
    } else {
    	printf("got logout()\n");
    }
    
    get_mailbox = dlsym(handle, "get_mailbox");
		
	dl_error = dlerror();
	if (dl_error || get_mailbox == NULL) {
    	printf("can not get get_mailbox()\n");
		return NULL;
    } else {
    	printf("got get_mailbox()\n");
    }
    
    add_mailbox = dlsym(handle, "add_mailbox");
		
	dl_error = dlerror();
	if (dl_error || add_mailbox == NULL) {
    	printf("can not get add_mailbox()\n");
		return NULL;
    } else {
    	printf("got add_mailbox()\n");
    }
    
    get_document = dlsym(handle, "get_document");
		
	dl_error = dlerror();
	if (dl_error || get_document == NULL) {
    	printf("can not get get_document()\n");
		return NULL;
    } else {
    	printf("got get_document()\n");
    }
    
    get_doc_attributes = dlsym(handle, "get_doc_attributes");
    
    dl_error = dlerror();
    if (dl_error || get_doc_attributes == NULL) {
        printf("can not get get_doc_attributes()\n");
        return NULL;
    } else {
        printf("got get_doc_attributes()\n");
    }
    
    del_document = dlsym(handle, "del_document");
    
    dl_error = dlerror();
    if (dl_error || del_document == NULL) {
        printf("can not get del_document()\ndl_error : %s\n", dl_error);
        return NULL;
    } else {
        printf("got del_document()\n");
    }
    
    return handle;
}

int close_api(void* handle) {
	if (handle != NULL) 
		dlclose(handle);
}

int test_get_mailbox(void* ptr_login) {
	struct mailbox_item_attrib* boxes;
	int i, nboxes;
	
	nboxes = (*get_mailbox)(ptr_login, (void*)&boxes);
	if(nboxes<0) {
		printf("error in get mailbox\n");
		return -1;
	}
		
	printf("id\tname\n");	
	for(i=0; i<nboxes; ++i) {
		printf("%d\t%s\n", boxes[i].box_id, boxes[i].name);
	}
}

int test_add_mailbox(void* ptr_login) {
	char box_name[255];
	char box_passwd[255];
	
	printf("================ test add mailbox ================\n");
	while(1) {
		printf("enter box name('-1' to exit)==>");
		scanf("%s", box_name);
		if(strcmp(box_name, "-1") == 0)
			break;
			
		printf("enter box passwd(>8 chars)==>");
		scanf("%s", box_passwd);
		
		if((*add_mailbox)(ptr_login, box_name, box_passwd) != 0)
			printf("error while add mailbox.\n");
		else
			test_get_mailbox(ptr_login);
	}
	return 0;
}

int test_get_document(short box_id, void* ptr_login) {
	struct document_item_attrib* docs;
	int i, ndocs;
	printf("get document (dll)\n");
	ndocs = (*get_document)(box_id, ptr_login, (void*)&docs);
	if(ndocs<0) {
		printf("error in get document\n");
		return -1;
	}
		
	printf("id\tname\n");	
	for(i=0; i<ndocs; ++i) {
		printf("%lld\t%s\n", docs[i].box_file_id, docs[i].name);
	}
    return 0;
}

int test_get_attributes(short box_id, const char* box_passwd, void* ptr_login) {
    struct document_item_attrib* docs;
    int i, ndocs;
    ndocs = (*get_document)(box_id, ptr_login, (void*)&docs);
    if(ndocs<0) {
        printf("error in get document(get attributes)\n");
        return -1;
    }
    for(i=0; i<ndocs; ++i) {
        printf("attributes in document : %s\n", docs[i].name);
        struct attr_key_value* attrs;
        int j, nattrs;
        nattrs = (*get_doc_attributes)(ptr_login, docs[i].box_file_id, box_passwd, (void*)&attrs);
        if(nattrs<0) {
            printf("error in get attributes\n");
            return -1;
        }
        printf("\tkey      :    value\n");   
        for(j=0; j<nattrs; ++j) {
            printf("\t%s : %s\n", attrs[j].key, attrs[j].value);
        }
    }
    return 0;
}

int test_del_document(short box_id, void* ptr_login) {
    int res = 0;
    long long doc_id;
    test_get_document(1, ptr_login);
    printf("================ test del mailbox ================\n");
    while(1) {
        printf("enter doc_id('-1' to skip)==>");
        scanf("%lld", &doc_id);       
        if(doc_id != -1) {
            res = del_document(ptr_login, doc_id, (short)box_id, "");
            test_get_document(1, ptr_login);
        }
        else
            break;
    }
    
    return 0;
}

int main(int argc, char** argv) {
	void* ptr_login = NULL;
	void* handle;
	
	if((handle=get_api()) == NULL)
		return -1;
	
	if((*is_sys_ok)()){
		printf("system is ok.\n");
		if( (ptr_login=(*login)("fxadmin","fxadmin")) == NULL){
			printf("can not login\n");
		}
		
		test_get_mailbox(ptr_login);
		
		test_add_mailbox(ptr_login);
		
		test_get_document(1, ptr_login);
        
        test_get_attributes(1, "", ptr_login);

        test_del_document(1, ptr_login);
            		
		(*logout)(ptr_login);
	}
	else
		printf("system isn't ready.\n");
		
	close_api(handle);
	return 0;
}

#else

//---------------------- use normal method ----------------------
#include "jal.h"

static int test_get_mailbox(void* ptr_login) {
	struct mailbox_item_attrib* boxes;
	int i, nboxes;
	
	nboxes = get_mailbox(ptr_login, (void*)&boxes);
	if(nboxes<0) {
		printf("error in get mailbox\n");
		return -1;
	}
		
	printf("id\tname\n");	
	for(i=0; i<nboxes; ++i) {
		printf("%d\t%s\n", boxes[i].box_id, boxes[i].name);
	}
}

static int test_add_mailbox_auto(
					void* ptr_login, 
					const char* box_name, 
					const char* box_passwd) {
	if(add_mailbox(ptr_login, box_name, box_passwd) != 0)
		printf("error while add mailbox.\n");
	else
		test_get_mailbox(ptr_login);
}

static int test_add_mailbox_with_id(
					void* ptr_login,
					short box_id,
					const char* box_name,
					const char* box_passwd) {
	if(add_mailbox_with_id(ptr_login, box_id, box_name, box_passwd) != 0)
		printf("error while add mailbox.\n");
	else
		test_get_mailbox(ptr_login);
}

int test_add_mailbox(void* ptr_login) {
	int   box_id=0;
	char  box_name[255];
	char  box_passwd[255];
	
	printf("================ test add mailbox ================\n");
	while(1) {
		printf("enter box name('-1' to exit)==>");
		scanf("%s", box_name);
		if(strcmp(box_name, "-1") == 0)
			break;
			
		printf("enter box passwd(>8 chars)==>");
		scanf("%s", box_passwd);
		
		printf("enter box_id('-1' to skip)==>");
		scanf("%d", &box_id);
		if(box_id == -1) {
			test_add_mailbox_auto(ptr_login, box_name, box_passwd);
		}
		else {
			test_add_mailbox_with_id(ptr_login, (short)box_id, box_name, box_passwd);
		}
	}
	return 0;
}

int test_del_mailbox(void* ptr_login) {
	int box_id=0;
	
	printf("================ test del mailbox ================\n");
	while(1) {
		printf("enter box_id('-1' to skip)==>");
		scanf("%d", &box_id);		// larry: Note Must be %c or crash!!!
		if(box_id != -1) {
			del_mailbox(ptr_login, (short)box_id);
			test_get_mailbox(ptr_login);
		}
		else
			break;
	}
	return 0;
}

int test_get_document(short box_id, void* ptr_login) {
	struct document_item_attrib* docs;
	int i, ndocs;
	
	printf("get document (include)\n");
	ndocs = get_document(box_id, ptr_login, (void*)&docs);
	if(ndocs<0) {
		printf("error in get document\n");
		return -1;
	}
	
    printf("ndocs = %d", ndocs);
	printf("doc: id\tname\n");	
	for(i=0; i<ndocs; i++) {
        printf("%d", i);
		printf("%llu\t%s\n", docs[i].box_file_id, docs[i].name);
	}
	return 0;
}

int test_get_attributes(short box_id, const char* box_passwd, void* ptr_login) {
    
    struct document_item_attrib* docs;
    int i, ndocs;
    
//    printf("get document (include)_(in attr)\n");
    ndocs = get_document(box_id, ptr_login, (void*)&docs);
    if(ndocs<0) {
        printf("error in get document\n");
        return -1;
    }
        
    for(i=0; i<ndocs; ++i) {
        printf("attributes in document : %s(%lld)\n", docs[i].name, docs[i].box_file_id);
        struct attr_key_value* attrs;
        int j, nattrs;
        
        printf("get attributes (include)\n");
        nattrs = get_doc_attributes(ptr_login, docs[i].box_file_id, box_passwd, (void*)&attrs);
        if(nattrs<0) {
            printf("error in get attributes\n");
            return -1;
        }
            
        printf("\tkey\tvalue\n");   
        for(j=0; j<nattrs; ++j) {
            printf("\t%s : %s\n", attrs[j].key, attrs[j].value);
        }
        
    }
    
    
    return 0;
}

int test_del_document(short box_id, void* ptr_login) {
    int res = 0;
    long long doc_id;
    test_get_document(1, ptr_login);
    printf("================ test del mailbox ================\n");
    while(1) {
        printf("enter doc_id('-1' to skip)==>");
        scanf("%lld", &doc_id);       
        if(doc_id != -1) {
            res = del_document(ptr_login, doc_id, (short)box_id, "");
            test_get_document(1, ptr_login);
        }
        else
            break;
    }
    
    return 0;
}

int test_del_one_doc(void* ptr_login) {
    struct document_item_attrib* docs;
    int i, ndocs;
    short box_id = 1;
//    printf("get document (include)_(in attr)\n");
    ndocs = get_document(box_id, ptr_login, (void*)&docs);
    if(ndocs<0) {
        printf("error in get document\n");
        return -1;
    } else if (ndocs == 0) {
        printf("there is no document in the mailbox : %d\n", box_id);
    }
    
    int res = 0;
    long long doc_id = docs[0].box_file_id;
    printf("document to deleted is : %lld\n", doc_id);
    res = del_document(ptr_login, doc_id, box_id, "");
    test_get_document(1, ptr_login);
    
    return 0;
    
}

int main(int argc, char** argv){
	void*  plogin;

	if(is_sys_ok())
	{
		if( (plogin=login("fxadmin","fxadmin")) == NULL){
			printf("can not login");
			return -1;
		}
		
		// list 1st time.
		if(test_get_mailbox(plogin) < 0){
			logout(plogin);
			return -1;
		}
		
		// add mailbox
		if(test_add_mailbox(plogin) < 0) {
			logout(plogin);
			return -1;
		}
		
		// del mailbox
		if(test_del_mailbox(plogin) < 0) {
			logout(plogin);
			return -1;
		}
		
		// list document
		if(test_get_document(1,plogin) < 0){
			logout(plogin);
			return -1;
		}
        
        // list attribute
        if(test_get_attributes(1, "", plogin)) {
            logout(plogin);
            return -1;
        }
        
        // del document
        if(test_del_document(1, plogin)) {
            logout(plogin);
            return -1;
        }
        
	
		logout(plogin);
		return 0;
	}
	return -1;
}

#endif //__USE_DLL__
//end
