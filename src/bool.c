#include "scm.h"

// globals
scmval scm_true;
scmval scm_false;

// constructor
scmval make_bool(bool b) {
    scmval v = make_val(SCM_TYPE_BOOL);
    v.b = b;
    return v;
}


// helpers
bool is_eqv(scmval x, scmval y) {
    if(type_of(x) != type_of(y))
        return false;
    if(is_eq(x, y))
        return true;
    bool r = false;
    if(is_string(x)) {
        r = !CORD_cmp(c_str(x), c_str(y));
    }
    return r;
}

bool is_equal(scmval x, scmval y) {
    if(type_of(x) != type_of(y))
        return false;
    if(is_eqv(x, y))
        return true;
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
    return scm_bool(is_bool(v));
}

static scmval scm_not(scmval v) {
    return scm_bool(is_false(v));
}

static scmval scm_eq(scmval x, scmval y) {
    return scm_bool(is_eq(x, y));
}

static scmval scm_eqv(scmval x, scmval y) {
    return scm_bool(is_eqv(x, y));
}

static scmval scm_equal(scmval x, scmval y) {
    return scm_bool(is_equal(x, y));
}

// initialization
void init_bool(scmval env) {
    scm_true  = make_bool(true);
    scm_false = make_bool(false);

    define(env, "boolean?", scm_boolean_p, arity_exactly(1));
    define(env, "not", scm_not, arity_exactly(1));
    define(env, "eq?", scm_eq, arity_exactly(2));
    define(env, "eqv?", scm_eqv, arity_exactly(2));
    define(env, "equal?", scm_equal, arity_exactly(2));
}

