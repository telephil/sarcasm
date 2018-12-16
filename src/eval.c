#include "scm.h"

static scmval scm_define;
static scmval scm_if;
static scmval scm_set;

void init_eval(scm_ctx_t* ctx) {
    scm_define = intern(ctx, "define");
    scm_if = intern(ctx, "if");
    scm_set = intern(ctx, "set!");
}

// (map (eval v e))
scmval map_eval(scm_ctx_t* ctx, scmval v, scmval e) {
    if(is_null(v))
        return scm_null;
    scmval r = eval(ctx, car(v), e);
    return cons(r, map_eval(ctx, cdr(v), e));
}

scmval eval(scm_ctx_t* ctx, scmval v, scmval e) {
    scmval r = v;

    if(is_symbol(v)) {
        r = lookup(ctx, v);
        if(is_undef(r))
            throw(ctx, error(intern(ctx, "error"), "undefined symbol '%s'", string_value(v)));
    } else if(is_pair(v) && !is_null(v)) {
        scmval s = car(v);
        if(is_eq(s, scm_define)) {
            scmval k = car(cdr(v));
            scmval a = eval(ctx, car(cdr(cdr(v))), e);
            dict_set(ctx->globals, k, a);
            r = a;
        } else if(is_eq(s, scm_if)) {
            scmval test = car(cdr(v));
            scmval t = car(cdr(cdr(v)));
            scmval f = car(cdr(cdr(cdr(v))));
            if(!is_false(eval(ctx, test, e)))
                r = eval(ctx, t, e);
            else
                r = eval(ctx, f, e);
        } else {
            scmval f = is_prim(s) ? s : eval(ctx, s, e);
            if(is_prim(f)) {
                scmval a = map_eval(ctx, cdr(v), e);
                r = apply(ctx, f, a);
            }
        }
    }

    return r;
}

