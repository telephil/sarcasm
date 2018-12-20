#include "scm.h"

// constructor
scmval make_vector(size_t size, scmval initial) {
    scm_vector_t* v = scm_new(scm_vector_t);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    for(int i = 0; i < size; i++) {
        v->elts[i] = initial;
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

scmval make_vector_from_list(int size, scmval l) {
    int i;
    scmval c;
    scm_vector_t* v = scm_new(scm_vector_t);
    if(size < 0)
        size = list_length(l);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    for(c = l, i = 0; !is_null(c); c = cdr(c), i++) {
        v->elts[i] = car(c);
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

// standard library
static scmval scm_vector_p(scmval v) {
    return scm_bool(is_vector(v));
}

static scmval scm_make_vector(scmval s, scmval i) {
    check_arg("make-vector", fixnum_c, s);
    opt_arg(i, make_fixnum(0));
    return make_vector(fixnum_value(s), i);
}

static scmval scm_vector(scmfix argc, scmval* argv) {
    scmval r;
    r = make_vector(argc, scm_undef);
    for(int i = 0; i < argc; i++) {
        vector_set(r, i, argv[i]);
    }
    return r;
}

static scmval scm_vector_ref(scmval v, scmval i) {
    check_arg("vector-ref", vector_c, v);
    check_arg("vector-ref", fixnum_c, i);
    scmval r;
    if(fixnum_value(i) < 0 || fixnum_value(i) >= vector_size(v))
        range_error("vector-ref", fixnum_value(i), vector_size(v));
    r = vector_ref(v, fixnum_value(i));
    return r;
}

static scmval scm_vector_set(scmval v, scmval i, scmval x) {
    check_arg("vector-set!", vector_c, v);
    check_arg("vector-set!", fixnum_c, i);
    if(fixnum_value(i) < 0 || fixnum_value(i) >= vector_size(v))
        range_error("vector-set!", fixnum_value(i), vector_size(v));
    vector_set(v, fixnum_value(i), x);
    return scm_undef;
}

static scmval scm_vector_to_list(scmval v) {
    check_arg("vector->list", vector_c, v);
    scmfix s = vector_size(v);
    scmval r = scm_null;
    for(int i = s - 1; i >= 0; i--) {
        r = cons(vector_ref(v, i), r);
    }
    return r;
}

static scmval scm_list_to_vector(scmval l) {
    check_arg("list->vector", list_c, l);
    scmfix s = list_length(l);
    scmval r = make_vector(s, scm_undef);
    for(int i = 0; i < s; i++, l = cdr(l)) {
        vector_set(r, i, car(l));
    }
    return r;
}

// initialization
void init_vector() {
    define("vector?", scm_vector_p, arity_exactly(1));
    define("make-vector", scm_make_vector, arity_or(1, 2));
    define("vector", scm_vector, arity_at_least(0));
    define("vector-ref", scm_vector_ref, arity_exactly(2));
    define("vector-set!", scm_vector_set, arity_exactly(3));
    define("vector->list", scm_vector_to_list, arity_exactly(1));
    define("list->vector", scm_list_to_vector, arity_exactly(1));
}

