#include "scm.h"

// constructor
scmval make_vector(size_t size, scmval initial) {
    scm_vector_t* v = GC_MALLOC(sizeof(scm_vector_t));
    v->size = size;
    v->elts = GC_MALLOC(size*sizeof(scmval));
    for(int i = 0; i < size; i++) {
        v->elts[i] = initial;
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

// standard library
static scmval scm_vector_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_vector(v))
        return scm_true;
    return scm_false;
}

static scmval scm_make_vector(scm_ctx_t* ctx) {
    scmval r, s, i;
    s = arg_ref(ctx, 0);
    i = arg_ref_opt(ctx, 1, make_fixnum(0));
    r = make_vector(get_fixnum(s), i);
    return r;
}

static scmval scm_vector(scm_ctx_t* ctx) {
    scmval r, *argv;
    int argc;
    arg_ref_list(ctx, &argc, &argv);
    r = make_vector(argc, scm_undef);
    for(int i = 0; i < argc; i++) {
        vector_set(r, i, argv[i]);
    }
    return r;
}

static scmval scm_vector_ref(scm_ctx_t* ctx) {
    scmval r, v, i;
    v = arg_ref(ctx, 0);
    i = arg_ref(ctx, 1);
    if(get_fixnum(i) < 0 || get_fixnum(i) >= vector_size(v)) {
        scmval e = range_error("vector-ref", get_fixnum(i), vector_size(v));
        throw(ctx, e);
    }
    r = vector_ref(v, get_fixnum(i));
    return r;
}

static scmval scm_vector_set(scm_ctx_t* ctx) {
    scmval v, i, x;
    v = arg_ref(ctx, 0);
    i = arg_ref(ctx, 1);
    x = arg_ref(ctx, 2);
    if(get_fixnum(i) < 0 || get_fixnum(i) >= vector_size(v)) {
        scmval e = range_error("vector-set!", get_fixnum(i), vector_size(v));
        throw(ctx, e);
    }
    vector_set(v, get_fixnum(i), x);
    return scm_undef;
}

static scmval scm_vector_to_list(scm_ctx_t* ctx) {
    scmval r, v;
    size_t s;
    v = arg_ref(ctx, 0);
    s = vector_size(v);
    r = scm_null;
    for(int i = s - 1; i >= 0; i--) {
        r = cons(vector_ref(v, i), r);
    }
    return r;
}

static scmval scm_list_to_vector(scm_ctx_t* ctx) {
    scmval r, l;
    size_t s;
    l = arg_ref(ctx, 0);
    s = list_length(l);
    r = make_vector(s, scm_undef);
    for(int i = 0; i < s; i++, l = cdr(l)) {
        vector_set(r, i, car(l));
    }
    return r;
}

// initialization
void init_vector(scm_ctx_t* ctx) {
    define(ctx, "vector?", scm_vector_p, arity_exactly(1), 1, any_c);
    define(ctx, "make-vector", scm_make_vector, arity_or(1, 2), 2, fixnum_c, any_c);
    define(ctx, "vector", scm_vector, arity_at_least(0), 1, any_c);
    define(ctx, "vector-ref", scm_vector_ref, arity_exactly(2), 2, vector_c, fixnum_c);
    define(ctx, "vector-set!", scm_vector_set, arity_exactly(3), 3, vector_c, fixnum_c, any_c);
    define(ctx, "vector->list", scm_vector_to_list, arity_exactly(1), 1, vector_c);
    define(ctx, "list->vector", scm_list_to_vector, arity_exactly(1), 1, list_c);
}

