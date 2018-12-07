#include "scm.h"

// globals
scmval scm_null;

// constructor
scmval make_pair(scmval car, scmval cdr) {
    scm_pair_t* l = GC_MALLOC(sizeof(scm_pair_t));
    l->car = car;
    l->cdr = cdr;
    return make_ptr(SCM_TYPE_PAIR, l);
}

// standard library
static scmval scm_pair_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_pair(v))
        return scm_true;
    return scm_false;
}

static scmval scm_list_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    for( ; !is_null(v); v = cdr(v)) {
        if(!is_pair(v)) {
            return scm_false;
        }
    }
    return scm_true;
}

static scmval scm_null_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_null(v)) {
        return scm_true;
    }
    return scm_false;
}

static scmval scm_cons(scm_ctx_t* ctx) {
    scmval r, h, t;
    h = arg_ref(ctx, 0);
    t = arg_ref(ctx, 1);
    r = make_pair(h, t);
    return r;
}

static scmval scm_car(scm_ctx_t* ctx) {
    scmval r, l;
    l = arg_ref(ctx, 0);
    r = car(l);
    return r;
}

static scmval scm_cdr(scm_ctx_t* ctx) {
    scmval r, l;
    l = arg_ref(ctx, 0);
    r = cdr(l);
    return r;
}

static scmval scm_length(scm_ctx_t* ctx) {
    scmval r, l;
    scm_fixnum_t i;
    l = arg_ref(ctx, 0);
    for(i = 0; !is_null(l); i++, l = cdr(l))
        ;
    r = make_fixnum(i);
    return r;
}

// initialization
void init_pair(scm_ctx_t* ctx) {
    scm_null  = make_val(SCM_TYPE_NULL);

    define(ctx, "pair?", scm_pair_p, arity_exactly(1), 1, any_c);
    define(ctx, "list?", scm_list_p, arity_exactly(1), 1, any_c);
    define(ctx, "null?", scm_null_p, arity_exactly(1), 1, any_c);
    define(ctx, "cons", scm_cons, arity_exactly(2), 2, any_c, any_c);
    define(ctx, "car", scm_car, arity_exactly(1), 1, list_c);
    define(ctx, "cdr", scm_cdr, arity_exactly(1), 1, list_c);
    define(ctx, "length", scm_length, arity_exactly(1), 1, list_c);
}

