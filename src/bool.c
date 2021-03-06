#include "scm.h"

// constructor
scmval make_bool(bool b) {
    scmval v = make_val(SCM_TYPE_BOOL);
    v.b = b;
    return v;
}

// helpers
bool is_eqv(scmval x, scmval y) {
    if(is_number(x) && is_number(y))
        return numeq(x,y);
    if(type_of(x) != type_of(y))
        return false;
    if(is_eq(x, y))
        return true;
    bool r = false;
    if(is_string(x)) {
        r = string_equal_p(x, y);
    }
    return r;
}

bool is_equal(scmval x, scmval y) {
    if(is_eqv(x, y))
        return true;
    if(type_of(x) != type_of(y))
        return false;
    if(is_pair(x)) {
        return is_equal(car(x), car(y)) && is_equal(cdr(x), cdr(y));
    } else if(is_vector(x)) {
        if(vector_size(x) != vector_size(y))
            return false;
        for(int i = 0; i < vector_size(x); i++) {
            if(!is_equal(vector_ref(x, i), vector_ref(y, i)))
                return false;
        }
        return true;
    } else if(is_bytevector(x)) {
        if(bytevector_size(x) != bytevector_size(y))
            return false;
        for(int i =0; i < bytevector_size(x); i++) {
            if(!is_eq(bytevector_ref(x, i), bytevector_ref(y, i)))
                return false;
        }
        return true;
    }
    return false;
}

// standard library
static scmval scm_boolean_p(scmval v) {
    return s_bool(is_bool(v));
}

scmval scm_not(scmval v) {
    return s_bool(is_false(v));
}

static scmval scm_eq(scmval x, scmval y) {
    return s_bool(is_eq(x, y));
}

static scmval scm_eqv(scmval x, scmval y) {
    return s_bool(is_eqv(x, y));
}

static scmval scm_equal(scmval x, scmval y) {
    return s_bool(is_equal(x, y));
}

static scmval scm_boolean_equal_p(int argc, scmval* argv) {
    check_args("boolean=?", bool_c, argc, argv);
    for(int i = 1; i < argc; i++) {
        if(!is_eq(argv[0], argv[i]))
            return scm_false;
    }
    return scm_true;
}

// initialization
void init_bool(scmval env) {
    define(env, "boolean?",  scm_boolean_p,       arity_exactly(1));
    define(env, "not",       scm_not,             arity_exactly(1));
    define(env, "boolean=?", scm_boolean_equal_p, arity_at_least(2));
    define(env, "eq?",       scm_eq,              arity_exactly(2));
    define(env, "eqv?",      scm_eqv,             arity_exactly(2));
    define(env, "equal?",    scm_equal,           arity_exactly(2));
}

