typedef struct kh_dict_s scm_dict_t;

// constructor
scm_dict_t* make_dict();

// accessors
void dict_set(scm_dict_t*, scmval, scmval);
scmval dict_ref(scm_dict_t*, scmval);
void dict_keys(scm_dict_t*, int*, scmval**);
void dict_copy(scm_dict_t*, scm_dict_t*);
void dict_foreach(scm_dict_t*, void(*callback)(scmval,scmval,scmval), scmval);

