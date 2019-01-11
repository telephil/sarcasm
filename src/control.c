#include "scm.h"

scmval scm_procedure_p(scmval obj) {
    return s_bool(is_procedure(obj));
}

scmval scm_callcc(scmval proc) {
    scmval r = scm_undef;
    scmval cont = make_continuation();
    if(setjmp(continuation_buf(cont))) {
        // back from call
        r = continuation_value(cont);
    } else {
        r = call(proc, list1(cont));
    }
    return r;
}


void init_control(scmval env) {
    define(env, "procedure?",                       scm_procedure_p,    arity_exactly(1));
    define(env, "call-with-current-continuation",   scm_callcc,         arity_exactly(1));
    define(env, "call/cc",                          scm_callcc,         arity_exactly(1));
}

