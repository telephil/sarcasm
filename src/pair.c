#include "scm.h"

// constructor
scmval make_pair(scmval car, scmval cdr) {
    scm_pair_t* l = scm_gc_malloc(sizeof(scm_pair_t));
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
    return s_bool(is_null(v));
}

static scmval scm_make_list(scmval s, scmval f) {
    check_arg("make-list", fixnum_c, s);
    opt_arg(f, scm_false);
    scmval r = scm_null;
    for(int i = 0; i < c_fix(s); i++) {
        r = cons(f, r);
    }
    return r;
}

static scmval scm_list(int argc, scmval* argv) {
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
    check_mutable("set-car!", "list", l);
    setcar(l, v);
    return scm_void;
}

static scmval scm_setcdr(scmval l, scmval v) {
    check_arg("set-cdr!", list_c, l);
    check_mutable("set-cdr!", "list", l);
    setcdr(l, v);
    return scm_void;
}

static scmval scm_length(scmval l) {
    if(is_null(l)) return scm_0; // FIXME
    check_arg("length", list_c, l);
    int i = 0;
    for(i = 0; !is_null(l); i++, l = cdr(l))
        ;
    return s_fix(i);
}

static scmval scm_append(int argc, scmval* argv) {
    if(argc == 0) return scm_null;
    check_args("append", pair_c, argc - 1, argv); // last argument can be anything
    scmval result = scm_null, t;
    for(int i = 0; i < argc; i++) {
        if(is_null(argv[i]))
            continue;
        if(i == (argc-1) && !is_pair(argv[i])) {
            if(is_null(result))
                return argv[i];
            setcdr(t, argv[i]);
        }
        for(scmval lst = argv[i]; !is_null(lst); lst = cdr(lst)) {
            scmval elt = car(lst);
            if(is_null(result)) {
                result = t = cons(elt, scm_null);
            } else {
                setcdr(t, cons(elt, scm_null));
                t = cdr(t);
            }
            if(!is_pair(cdr(lst))) {
                if(i < (argc-1))
                    error(type_error_type, "append: %s is not a proper list", scm_to_cstr(argv[i]));
                else {
                    setcdr(t, cdr(lst));
                    break;
                }
            }
        }
    }
    return result;
}

static scmval scm_list_set(scmval lst, scmval k, scmval obj) {
    check_arg("list-set!", list_c, lst);
    check_arg("list-set!", fixnum_c, k);
    check_range("list-set!", c_fix(k), 0, list_length(lst));
    check_mutable("list-set!", "list", lst);
    scmval p = lst;
    for(int i = 0; i < c_fix(k); i++)
        p = cdr(p);
    setcar(p, obj);
    return scm_void;
}

// initialization
void init_pair(scmval env) {
    define(env, "pair?",        scm_pair_p,     arity_exactly(1));
    define(env, "list?",        scm_list_p,     arity_exactly(1));
    define(env, "null?",        scm_null_p,     arity_exactly(1));
    define(env, "make-list",    scm_make_list,  arity_or(1, 2));
    define(env, "list",         scm_list,       arity_at_least(0));
    define(env, "cons",         scm_cons,       arity_exactly(2));
    define(env, "car",          scm_car,        arity_exactly(1));
    define(env, "cdr",          scm_cdr,        arity_exactly(1));
    define(env, "set-car!",     scm_setcar,     arity_exactly(2));
    define(env, "set-cdr!",     scm_setcdr,     arity_exactly(2));
    define(env, "length",       scm_length,     arity_exactly(1));
    define(env, "append",       scm_append,     arity_at_least(0));
    define(env, "list-set!",    scm_list_set,   arity_exactly(3));
}

