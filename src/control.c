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

scmval scm_dynamic_wind(scmval before, scmval thunk, scmval after) {
    scmval ret = scm_undef;
    call(before, scm_null);
    if(!setjmp(scm_g_errbuf)) {
        ret = call(thunk, scm_null);
    }
    call(after, scm_null);
    return ret;
}

void init_control(scmval env) {
    define(env, "procedure?",                       scm_procedure_p,    arity_exactly(1));
    define(env, "call-with-current-continuation",   scm_callcc,         arity_exactly(1));
    define(env, "call/cc",                          scm_callcc,         arity_exactly(1));
    define(env, "dynamic-wind",                     scm_dynamic_wind,   arity_exactly(3));
}

