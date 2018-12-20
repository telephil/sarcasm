#include "scm.h"

// globals
scmval scm_null;

// constructor
scmval make_pair(scmval car, scmval cdr) {
    scm_pair_t* l = scm_new(scm_pair_t);
    l->car = car;
    l->cdr = cdr;
    return make_ptr(SCM_TYPE_PAIR, l);
}

// standard library
static scmval scm_pair_p(scmval v) {
    if(is_pair(v) && !is_null(v))
        return scm_true;
    return scm_false;
}

static scmval scm_list_p(scmval v) {
    for( ; !is_null(v); v = cdr(v)) {
        if(!is_pair(v)) {
            return scm_false;
        }
    }
    return scm_true;
}

static scmval scm_null_p(scmval v) {
    return scm_bool(is_null(v));
}

static scmval scm_make_list(scmval s, scmval f) {
    check_arg("make-list", fixnum_c, s);
    opt_arg(f, scm_false);
    scmval r = scm_null;
    for(int i = 0; i < fixnum_value(s); i++) {
        r = cons(f, r);
    }
    return r;
}

static scmval scm_list(scmfix argc, scmval* argv) {
    scmval r = scm_null;
    for(int i = argc - 1; i >= 0; i--) {
        r = cons(argv[i], r);
    }
    return r;
}

static scmval scm_cons(scmval h, scmval t) {
    return make_pair(h, t);
}

static scmval scm_car(scmval l) {
    check_arg("car", list_c, l);
    return car(l);
}

static scmval scm_cdr(scmval l) {
    check_arg("cdr", list_c, l);
    return cdr(l);
}

static scmval scm_setcar(scmval l, scmval v) {
    check_arg("set-car!", list_c, l);
    setcar(l, v);
    return scm_undef;
}

static scmval scm_setcdr(scmval l, scmval v) {
    check_arg("set-cdr!", list_c, l);
    setcdr(l, v);
    return scm_undef;
}

static scmval scm_length(scmval l) {
    check_arg("length", list_c, l);
    scmval r;
    scmfix i;
    for(i = 0; !is_null(l); i++, l = cdr(l))
        ;
    r = make_fixnum(i);
    return r;
}

// initialization
void init_pair() {
    scm_null  = make_val(SCM_TYPE_NULL);

    define("pair?", scm_pair_p, arity_exactly(1));
    define("list?", scm_list_p, arity_exactly(1));
    define("null?", scm_null_p, arity_exactly(1));
    define("make-list", scm_make_list, arity_or(1, 2));
    define("list", scm_list, arity_at_least(0));
    define("cons", scm_cons, arity_exactly(2));
    define("car", scm_car, arity_exactly(1));
    define("cdr", scm_cdr, arity_exactly(1));
    define("set-car!", scm_setcar, arity_exactly(2));
    define("set-cdr!", scm_setcdr, arity_exactly(2));
    define("length", scm_length, arity_exactly(1));
}

