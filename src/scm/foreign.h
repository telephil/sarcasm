#include <ffi.h>

typedef struct scm_foreign_lib  scm_foreign_lib_t;
typedef struct scm_foreign_obj  scm_foreign_obj_t;
typedef struct scm_foreign_type scm_foreign_type_t;
typedef struct scm_foreign_ptr  scm_foreign_ptr_t;

struct scm_foreign_lib {
    char* name;
    void* handle;
};

struct scm_foreign_obj {
    char*   name;
    scmval  ret;
    scmval  args;
    void*   handle;
    ffi_cif cif;
};

struct scm_foreign_type {
    char*       name;
    short       code;
    ffi_type*   type;
};

struct scm_foreign_ptr {
    void*   ptr;
    scmval  type;
};

#define FOREIGN_TYPE_VOID           0
#define FOREIGN_TYPE_UINT8          1
#define FOREIGN_TYPE_SINT8          2
#define FOREIGN_TYPE_UINT16         3
#define FOREIGN_TYPE_SINT16         4
#define FOREIGN_TYPE_UINT32         5
#define FOREIGN_TYPE_SINT32         6
#define FOREIGN_TYPE_UINT64         7
#define FOREIGN_TYPE_SINT64         8
#define FOREIGN_TYPE_FLOAT          9
#define FOREIGN_TYPE_DOUBLE         10
#define FOREIGN_TYPE_UCHAR          11
#define FOREIGN_TYPE_SCHAR          12
#define FOREIGN_TYPE_USHORT         13
#define FOREIGN_TYPE_SSHORT         14
#define FOREIGN_TYPE_UINT           15
#define FOREIGN_TYPE_SINT           16
#define FOREIGN_TYPE_ULONG          17
#define FOREIGN_TYPE_SLONG          18
#define FOREIGN_TYPE_LONGDOUBLE     19
#define FOREIGN_TYPE_POINTER        20
#define FOREIGN_TYPE_STRING         21

// constructors
scmval make_foreign_lib(const char*, void*);
scmval make_foreign_obj(const char*, scmval, scmval, void*, ffi_cif);
scmval make_foreign_type(const char*, short, ffi_type*);
scmval make_foreign_ptr(void*, scmval);

// predicates
static inline bool is_foreign_lib(scmval obj) { return type_of(obj) == SCM_TYPE_FOREIGN_LIB; }
static inline bool is_foreign_obj(scmval obj) { return type_of(obj) == SCM_TYPE_FOREIGN_OBJ; }
static inline bool is_foreign_ptr(scmval obj) { return type_of(obj) == SCM_TYPE_FOREIGN_PTR; }
static inline bool is_foreign_type(scmval obj) { return type_of(obj) == SCM_TYPE_FOREIGN_TYPE; }

// contracts
define_contract(foreign_lib_c, "foreign library", is_foreign_lib);
define_contract(foreign_obj_c, "foreign object", is_foreign_obj);
define_contract(foreign_ptr_c, "foreign pointer", is_foreign_ptr);
define_contract(foreign_type_c, "foreign type", is_foreign_type);

// accessors
static inline scm_foreign_lib_t* get_foreign_lib(scmval v) { return (scm_foreign_lib_t*)v.o; }
static inline char*              foreign_lib_name(scmval v) { return get_foreign_lib(v)->name; }
static inline void*              foreign_lib_handle(scmval v) { return get_foreign_lib(v)->handle; }

static inline scm_foreign_obj_t* get_foreign_obj(scmval v) { return (scm_foreign_obj_t*)v.o; }
static inline char*              foreign_obj_name(scmval v) { return get_foreign_obj(v)->name; }
static inline scmval             foreign_obj_ret(scmval v) { return get_foreign_obj(v)->ret; }
static inline scmval             foreign_obj_args(scmval v) { return get_foreign_obj(v)->args; }
static inline void*              foreign_obj_handle(scmval v) { return get_foreign_obj(v)->handle; }
static inline ffi_cif            foreign_obj_cif(scmval v) { return get_foreign_obj(v)->cif; }

static inline scm_foreign_type_t* get_foreign_type(scmval v) { return (scm_foreign_type_t*)v.o; }
static inline char*     foreign_type_name(scmval v) { return get_foreign_type(v)->name; }
static inline short     foreign_type_code(scmval v) { return get_foreign_type(v)->code; }
static inline ffi_type* foreign_type_type(scmval v) { return get_foreign_type(v)->type; }

static inline scm_foreign_ptr_t* get_foreign_ptr(scmval v) { return (scm_foreign_ptr_t*)v.o; }
static inline void*     foreign_ptr_ptr(scmval v) { return get_foreign_ptr(v)->ptr; }
static inline void      set_foreign_ptr_ptr(scmval v, void* p) { get_foreign_ptr(v)->ptr = p; }
static inline scmval    foreign_ptr_type(scmval v) { return get_foreign_ptr(v)->type; }

// foreign pointer
static inline scmval s_ptr(void* o) { return make_foreign_ptr(o, scm_undef); }
static inline void*  c_ptr(scmval v) { return foreign_ptr_ptr(v); }

scmval foreign_call(scmval, int, scmval*);


