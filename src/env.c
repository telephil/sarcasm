#include "scm.h"

// constructor
scmval make_env(scmval n) {
    scm_env_t* e = scm_new(scm_env_t);
    e->bindings= make_dict();
    e->next = n;
    return make_ptr(SCM_TYPE_ENV, e);
}

// utilities
void define(scm_ctx_t* ctx, const char* name, scm_prim_fun fun, arity_t arity, int n, ...) {
    va_list ap;
    va_start(ap, n);
    scmval prim = make_primv(name, fun, arity, n, ap);
    va_end(ap);
    dict_set(ctx->globals, intern(ctx, name), prim);
}

scmval lookup(scm_ctx_t* ctx, scmval e, scmval s) {
    scmval v;
    while(!is_undef(e)) {
        v = dict_ref(env_bindings(e), s);
        if(!is_undef(v))
            return v;
        e = env_next(e);
    }

    return dict_ref(ctx->globals, s);
}

void bind(scm_ctx_t* ctx, scmval e, scmval s, scmval v) {
    dict_set(env_bindings(e), s, v);
}


