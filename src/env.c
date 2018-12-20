#include "scm.h"

// constructor
scmval make_env(scmval n) {
    scm_env_t* e = scm_new(scm_env_t);
    e->bindings= make_dict();
    e->next = n;
    return make_ptr(SCM_TYPE_ENV, e);
}

// utilities
void define(const char* name, subr fun, arity_t arity) {
    scmval s = make_subr(name, fun, arity);
    dict_set(scm_context.globals, intern(name), s);
}

scmval lookup(scmval e, scmval s) {
    scmval v;
    while(!is_undef(e)) {
        v = dict_ref(env_bindings(e), s);
        if(!is_undef(v))
            return v;
        e = env_next(e);
    }

    return dict_ref(scm_context.globals, s);
}

void bind(scmval e, scmval s, scmval v) {
    dict_set(env_bindings(e), s, v);
}


