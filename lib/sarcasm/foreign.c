#include <dlfcn.h>
#include <ffi.h>
#include <scm.h>

static scmval scm_foreign_lib_p(scmval obj) {
    return s_bool(is_foreign_lib(obj));
}

static scmval scm_foreign_obj_p(scmval obj) {
    return s_bool(is_foreign_obj(obj));
}

static scmval scm_foreign_type_p(scmval obj) {
    return s_bool(is_foreign_type(obj));
}

static scmval scm_foreign_ptr_p(scmval obj) {
    return s_bool(is_foreign_ptr(obj));
}

static scmval scm_foreign_lib(scmval sname) {
    const char* name = c_str(sname);
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
    const char* name = c_str(sname);
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

static scmval scm_ptr_alloc(scmval type) {
    check_arg("ptr-alloc", foreign_type_c, type);
    void* p = malloc(foreign_type_type(type)->size);
    return make_foreign_ptr(p, type);
}

static scmval scm_ptr_free(scmval p) {
    check_arg("ptr-free", foreign_ptr_c, p);
    free(foreign_ptr_ptr(p));
    set_foreign_ptr_ptr(p, NULL);
    return scm_void;
}

static scmval scm_ptr_deref(scmval ptr) {
    check_arg("ptr-deref", foreign_ptr_c, ptr);
    if(is_undef(foreign_ptr_type(ptr)))
        error(scm_undef, "cannot dereference untyped pointer");
    if(foreign_ptr_ptr(ptr) == NULL)
        error(scm_undef, "cannot dereference a null pointer");
    void* v = foreign_ptr_ptr(ptr);
    scmval res = scm_undef;
    switch(foreign_type_code(foreign_ptr_type(ptr))) {
        case FOREIGN_TYPE_UINT8:    res = s_fix(*(uint8_t*)v);  break;
        case FOREIGN_TYPE_SINT8:    res = s_fix(*(int8_t*)v);  break;
        case FOREIGN_TYPE_UINT16:   res = s_fix(*(uint16_t*)v); break;
        case FOREIGN_TYPE_SINT16:   res = s_fix(*(int16_t*)v); break;
        case FOREIGN_TYPE_UINT32:   res = s_fix(*(uint32_t*)v); break;
        case FOREIGN_TYPE_SINT32:   res = s_fix(*(int32_t*)v); break;
        case FOREIGN_TYPE_UINT64:   res = s_fix(*(uint64_t*)v); break;
        case FOREIGN_TYPE_SINT64:   res = s_fix(*(int64_t*)v); break;
        case FOREIGN_TYPE_FLOAT:    res = s_flo(*(float*)v);   break;
        case FOREIGN_TYPE_DOUBLE:   res = s_flo(*(double*)v);   break;
        case FOREIGN_TYPE_UCHAR:    res = s_char(*(unsigned char*)v); break;
        case FOREIGN_TYPE_SCHAR:    res = s_char(*(char*)v);  break;
        case FOREIGN_TYPE_USHORT:   res = s_fix(*(unsigned short*)v);  break;
        case FOREIGN_TYPE_SSHORT:   res = s_fix(*(short*)v);   break;
        case FOREIGN_TYPE_UINT:     res = s_fix(*(unsigned int*)v);  break;
        case FOREIGN_TYPE_SINT:     res = s_fix(*(int*)v);   break;
        case FOREIGN_TYPE_ULONG:    res = s_fix(*(unsigned long*)v);  break;
        case FOREIGN_TYPE_SLONG:    res = s_fix(*(long*)v);   break;
    }
    return res;
}

void init_module(scmval env) {
    define(env, "foreign-lib?",     scm_foreign_lib_p,  arity_exactly(1));
    define(env, "foreign-obj?",     scm_foreign_obj_p,  arity_exactly(1));
    define(env, "foreign-type?",    scm_foreign_type_p, arity_exactly(1));
    define(env, "foreign-ptr?",     scm_foreign_ptr_p,  arity_exactly(1));
    define(env, "foreign-lib",      scm_foreign_lib,    arity_exactly(1));
    define(env, "foreign-obj",      scm_foreign_obj,    arity_at_least(3));
    define(env, "ptr-alloc",        scm_ptr_alloc,      arity_exactly(1));
    define(env, "ptr-free",         scm_ptr_free,       arity_exactly(1));
    define(env, "ptr-deref",        scm_ptr_deref,      arity_exactly(1));

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

