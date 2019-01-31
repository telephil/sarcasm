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
    lib->name   = scm_gc_strdup(name);
    lib->handle = handle;
    return make_ptr(SCM_TYPE_FOREIGN_LIB, lib);
}

scmval make_foreign_obj(const char* name, scmval ret, scmval args, void* handle, ffi_cif cif) {
    scm_foreign_obj_t* obj = scm_new(scm_foreign_obj_t);
    obj->name   = scm_gc_strdup(name);
    obj->ret    = ret;
    obj->args   = args;
    obj->handle = handle;
    obj->cif    = cif;
    return make_ptr(SCM_TYPE_FOREIGN_OBJ, obj);
}

scmval make_foreign_type(const char* name, short code, ffi_type* type) {
    scm_foreign_type_t* t = scm_new(scm_foreign_type_t);
    t->name = scm_gc_strdup(name);
    t->code = code;
    t->type = type;
    return make_ptr(SCM_TYPE_FOREIGN_TYPE, t);
}

scmval make_foreign_ptr(void* ptr, scmval type) {
    scm_foreign_ptr_t* p = scm_new(scm_foreign_ptr_t);
    p->ptr  = ptr;
    p->type = type;
    return make_ptr(SCM_TYPE_FOREIGN_PTR, p);
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
        case FOREIGN_TYPE_STRING:   res.p   = c_str(v);                     break;
        case FOREIGN_TYPE_POINTER:  res.p   = c_ptr(v);                     break;
    }
    return res;
}
