#include "scm.h"

// globals
scmval   scm_g_lasterr;
jmp_buf  scm_g_errbuf;
scmval range_error_type;
scmval arity_error_type;
scmval type_error_type;

// constructor
scmval make_error(scmval type, scmval message) {
    scm_error_t* e = scm_new(scm_error_t);
    e->type = type;
    e->message = message;
    return make_ptr(SCM_TYPE_ERROR, e);
}

// standard library
void raise(scmval e) {
    scm_g_lasterr = e;
    longjmp(scm_g_errbuf, 1);
}

// initialization
void init_errors(scmval env) {
    type_error_type  = intern("type-error");
    range_error_type = intern("range-error");
    arity_error_type = intern("arity-error");
}

