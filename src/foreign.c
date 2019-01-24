#include <dlfcn.h>
#include "scm.h"

typedef union value value;

union value {
    uint8_t         u8;
    int8_t          s8;
    uint16_t        u16;
    int16_t         s16;
    uint32_t        u32;
    int32_t         s32;
    uint64_t        u64;
    int64_t         s64;
    float           f;
    double          d;
    unsigned char   uc;
    char            c;
    unsigned short  us;
    short           s;
    unsigned int    ui;
    int             i;
    unsigned long   ul;
    signed long     l;
    void*           p;
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

////////////////////////////////////////////////////////////////////////////////
// FORWARD DECLARATIONS
////////////////////////////////////////////////////////////////////////////////
static scmval s_val(value, scmval);
static value  c_val(scmval, scmval);

////////////////////////////////////////////////////////////////////////////////
// CONSTRUCTORS
////////////////////////////////////////////////////////////////////////////////
scmval make_foreign_lib(const char* name, void* handle) {
    scm_foreign_lib_t* lib = scm_new(scm_foreign_lib_t);
    lib->name   = GC_STRDUP(name);
    lib->handle = handle;
    return make_ptr(SCM_TYPE_FOREIGN_LIB, lib);
}

scmval make_foreign_obj(const char* name, scmval ret, scmval args, void* handle, ffi_cif cif) {
    scm_foreign_obj_t* obj = scm_new(scm_foreign_obj_t);
    obj->name   = GC_STRDUP(name);
    obj->ret    = ret;
    obj->args   = args;
    obj->handle = handle;
    obj->cif    = cif;
    return make_ptr(SCM_TYPE_FOREIGN_OBJ, obj);
}

scmval make_foreign_type(const char* name, short code, ffi_type* type) {
    scm_foreign_type_t* t = scm_new(scm_foreign_type_t);
    t->name = GC_STRDUP(name);
    t->code = code;
    t->type = type;
    return make_ptr(SCM_TYPE_FOREIGN_TYPE, t);
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_foreign_lib(scmval sname) {
    const char* name = c_cstr(sname);
    void* handle = dlopen(name, RTLD_LAZY);
    if(handle == NULL)
        error(scm_undef, "could not open library %s: %s", name, dlerror());
    return make_foreign_lib(name, handle);
}

static scmval scm_foreign_obj(int argc, scmval* argv) {
    check_arg("foreign-obj", foreign_lib_c, argv[0]);
    check_arg("foreign-obj", string_c, argv[1]);
    for(int i = 2; i < argc; i++)
        check_arg("foreign-obj", foreign_type_c, argv[i]);
    scmval flib  = argv[0];
    scmval sname = argv[1];
    scmval ret   = argv[2];
    const char* name = c_cstr(sname);
    void* handle = dlsym(foreign_lib_handle(flib), name);
    if(handle == NULL)
        error(scm_undef, "could not find symbol %s in library (%s)", name, dlerror());
    ffi_type *rtype = foreign_type_type(ret);
    ffi_type **atypes = NULL;
    scmval args = scm_null;
    int len = argc - 3;
    if(len > 0) {
        atypes = scm_new_array(len, ffi_type*);
        for(int i = 3; i < argc; i++) {
            if(foreign_type_code(argv[i]) == FOREIGN_TYPE_VOID)
                error(scm_undef, "void is not a valid argument type");
            atypes[i - 3] = foreign_type_type(argv[i]);
            push(argv[i], args);
        }
        args = list_reverse(args);
    }
    ffi_cif cif;
    ffi_status status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, len, rtype, atypes);
    if(status != FFI_OK)
        error(scm_undef, "could not create foreign object");
    return make_foreign_obj(name, ret, args, handle, cif);
}

void init_foreign(scmval env) {
    define(env, "foreign-lib", scm_foreign_lib, arity_exactly(1));
    define(env, "foreign-obj", scm_foreign_obj, arity_at_least(3));

#define define_foreign_type(env,sname,name,code,type) \
    dict_set(env_globals(env), intern(sname), make_foreign_type(name, code, type))

    define_foreign_type(env, "_void",   "void",     FOREIGN_TYPE_VOID,      &ffi_type_void);
    define_foreign_type(env, "_uint8",  "uint8",    FOREIGN_TYPE_UINT8,     &ffi_type_uint8);
    define_foreign_type(env, "_int8",   "int8",     FOREIGN_TYPE_SINT8,     &ffi_type_sint8);
    define_foreign_type(env, "_uint16", "uint16",   FOREIGN_TYPE_UINT16,    &ffi_type_uint16);
    define_foreign_type(env, "_int16",  "int16",    FOREIGN_TYPE_SINT16,    &ffi_type_sint16);
    define_foreign_type(env, "_uint32", "uint32",   FOREIGN_TYPE_UINT32,    &ffi_type_uint32);
    define_foreign_type(env, "_int32",  "int32",    FOREIGN_TYPE_SINT32,    &ffi_type_sint32);
    define_foreign_type(env, "_uint64", "uint64",   FOREIGN_TYPE_UINT64,    &ffi_type_uint64);
    define_foreign_type(env, "_int64",  "int64",    FOREIGN_TYPE_SINT64,    &ffi_type_sint64);
    define_foreign_type(env, "_float",  "float",    FOREIGN_TYPE_FLOAT,     &ffi_type_float);
    define_foreign_type(env, "_double", "double",   FOREIGN_TYPE_DOUBLE,    &ffi_type_double);
    define_foreign_type(env, "_uchar",  "uchar",    FOREIGN_TYPE_UCHAR,     &ffi_type_uchar);
    define_foreign_type(env, "_char",   "char",     FOREIGN_TYPE_SCHAR,     &ffi_type_schar);
    define_foreign_type(env, "_ushort", "ushort",   FOREIGN_TYPE_USHORT,    &ffi_type_ushort);
    define_foreign_type(env, "_short",  "short",    FOREIGN_TYPE_SSHORT,    &ffi_type_sshort);
    define_foreign_type(env, "_uint",   "uint",     FOREIGN_TYPE_UINT,      &ffi_type_uint);
    define_foreign_type(env, "_int",    "int",      FOREIGN_TYPE_SINT,      &ffi_type_sint);
    define_foreign_type(env, "_ulong",  "ulong",    FOREIGN_TYPE_ULONG,     &ffi_type_ulong);
    define_foreign_type(env, "_slong",  "slong",    FOREIGN_TYPE_SLONG,     &ffi_type_slong);
    define_foreign_type(env, "_ptr",    "pointer",  FOREIGN_TYPE_POINTER,   &ffi_type_pointer);
    define_foreign_type(env, "_string", "string",   FOREIGN_TYPE_STRING,    &ffi_type_pointer);
}

////////////////////////////////////////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////////////////////////////////////////
typedef void(*func)(void);
scmval foreign_call(scmval fobj, int argc, scmval* argv) {
    ffi_cif cif = foreign_obj_cif(fobj);
    func fn = (func)foreign_obj_handle(fobj);
    value cargs[32];
    void* vargs[32];
    if(argc > 0) {
        scmval args = foreign_obj_args(fobj);
        for(int i = 0; i < argc; i++) {
            if(is_null(args))
                error(scm_undef, "too many arguments in foreign call");
            cargs[i] = c_val(argv[i], car(args));
            vargs[i] = &(cargs[i]);
            args = cdr(args);
        }
    }
    value result;
    ffi_call(&cif, fn, &result, vargs);
    return s_val(result, foreign_obj_ret(fobj));
}

static scmval s_val(value v, scmval type) {
    scmval res = scm_undef;
    switch(foreign_type_code(type)) {
        case FOREIGN_TYPE_VOID:     res = scm_void;     break;
        case FOREIGN_TYPE_UINT8:    res = s_fix(v.u8);  break;
        case FOREIGN_TYPE_SINT8:    res = s_fix(v.s8);  break;
        case FOREIGN_TYPE_UINT16:   res = s_fix(v.u16); break;
        case FOREIGN_TYPE_SINT16:   res = s_fix(v.s16); break;
        case FOREIGN_TYPE_UINT32:   res = s_fix(v.u32); break;
        case FOREIGN_TYPE_SINT32:   res = s_fix(v.s32); break;
        case FOREIGN_TYPE_UINT64:   res = s_fix(v.u64); break;
        case FOREIGN_TYPE_SINT64:   res = s_fix(v.s64); break;
        case FOREIGN_TYPE_FLOAT:    res = s_flo(v.f);   break;
        case FOREIGN_TYPE_DOUBLE:   res = s_flo(v.d);   break;
        case FOREIGN_TYPE_UCHAR:    res = s_char(v.uc); break;
        case FOREIGN_TYPE_SCHAR:    res = s_char(v.c);  break;
        case FOREIGN_TYPE_USHORT:   res = s_fix(v.us);  break;
        case FOREIGN_TYPE_SSHORT:   res = s_fix(v.s);   break;
        case FOREIGN_TYPE_UINT:     res = s_fix(v.ui);  break;
        case FOREIGN_TYPE_SINT:     res = s_fix(v.i);   break;
        case FOREIGN_TYPE_ULONG:    res = s_fix(v.ul);  break;
        case FOREIGN_TYPE_SLONG:    res = s_fix(v.l);   break;
        case FOREIGN_TYPE_POINTER:  res = s_ptr(v.p);   break;
        case FOREIGN_TYPE_STRING:   res = s_str(v.p);   break;
    }
    return res;
}

static value  c_val(scmval v, scmval type) {
    value res = {.i = 0};
    switch(foreign_type_code(type)) {
        case FOREIGN_TYPE_UINT8:    res.u8  = (uint8_t)c_fix(v);            break;
        case FOREIGN_TYPE_SINT8:    res.s8  = (int8_t)c_fix(v);             break;
        case FOREIGN_TYPE_UINT16:   res.u16 = (uint16_t)c_fix(v);           break;
        case FOREIGN_TYPE_SINT16:   res.s16 = (int16_t)c_fix(v);            break;
        case FOREIGN_TYPE_UINT32:   res.u32 = (uint32_t)c_fix(v);           break;
        case FOREIGN_TYPE_SINT32:   res.s32 = (int32_t)c_fix(v);            break;
        case FOREIGN_TYPE_UINT64:   res.u64 = (uint64_t)c_fix(v);           break;
        case FOREIGN_TYPE_SINT64:   res.s64 = (int64_t)c_fix(v);            break;
        case FOREIGN_TYPE_FLOAT:    res.f   = (float)c_flo(v);              break;
        case FOREIGN_TYPE_DOUBLE:   res.d   = (double)c_flo(v);             break;
        case FOREIGN_TYPE_UCHAR:    res.uc  = (unsigned char)c_char(v);     break;
        case FOREIGN_TYPE_SCHAR:    res.c   = (char)c_char(v);              break;
        case FOREIGN_TYPE_USHORT:   res.us  = (unsigned short)c_fix(v);     break;
        case FOREIGN_TYPE_SSHORT:   res.s   = (short)c_fix(v);              break;
        case FOREIGN_TYPE_UINT:     res.ui  = (unsigned int)c_fix(v);       break;
        case FOREIGN_TYPE_SINT:     res.i   = (int)c_fix(v);                break;
        case FOREIGN_TYPE_ULONG:    res.ul  = (unsigned long)c_fix(v);      break; // error
        case FOREIGN_TYPE_SLONG:    res.l   = (long)c_fix(v);               break;
        case FOREIGN_TYPE_POINTER:  res.p   = c_ptr(v);                     break;
        case FOREIGN_TYPE_STRING:   res.p   = c_cstr(v);                    break;
    }
    return res;
}
