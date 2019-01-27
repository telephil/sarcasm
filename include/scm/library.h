typedef struct scm_library scm_library_t;

struct scm_library {
    scmval      name;
    scm_dict_t* symbols;
    scmval      exports;
};
// constructor
scmval make_library(scmval);
scmval make_core_library(scmval);
// predicates
static inline bool is_library(scmval obj) { return type_of(obj) == SCM_TYPE_LIBRARY; }
// accessors
static inline scm_library_t* get_library(scmval obj) { return (scm_library_t*)obj.o; }
static inline scmval         library_name(scmval obj) { return get_library(obj)->name; }
static inline scm_dict_t*    library_symbols(scmval obj) { return get_library(obj)->symbols; }
static inline scmval         library_exports(scmval obj) { return get_library(obj)->exports; }
static inline void           library_add_export(scmval obj, scmval sym) {
    get_library(obj)->exports = cons(sym, get_library(obj)->exports);
}
// helpers
static inline scmval lib(const char* x, const char* y) { return list2(intern(x), intern(y)); }
void import_c_module(scmval, scmval);
// initialization
void init_library(scmval env);
// standard library
scmval load_library(scmval, scmval);
void   register_library(scmval);

static inline char* library_path() {
    static char* path = NULL;
    if(path == NULL) {
        path = getenv("SCM_LIBRARY_PATH");
        if(path == NULL)
            path = SCM_LIBRARY_PATH;
    }
    return path;
}

