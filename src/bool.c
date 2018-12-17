#include "scm.h"

// globals
scmval scm_true;
scmval scm_false;

// constructor
scmval make_bool(scm_bool_t b) {
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
        r = !CORD_cmp(string_value(x), string_value(y));
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
    }
    return false;
}

// standard library
static scmval scm_boolean_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_bool(v))
        return scm_true;
    return scm_false;
}

static scmval scm_not(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_false(v))
        return scm_true;
    return scm_false;
}

static scmval scm_eq(scm_ctx_t* ctx) {
    scmval x, y;
    x = arg_ref(ctx, 0);
    y = arg_ref(ctx, 1);
    if(is_eq(x, y))
        return scm_true;
    return scm_false;
}

static scmval scm_eqv(scm_ctx_t* ctx) {
    scmval x, y;
    x = arg_ref(ctx, 0);
    y = arg_ref(ctx, 1);
    if(is_eqv(x, y))
        return scm_true;
    return scm_false;
}

static scmval scm_equal(scm_ctx_t* ctx) {
    scmval x, y;
    x = arg_ref(ctx, 0);
    y = arg_ref(ctx, 1);
    if(is_equal(x, y))
        return scm_true;
    return scm_false;
}

// initialization
void init_bool(scm_ctx_t* ctx) {
    scm_true  = make_bool(true);
    scm_false = make_bool(false);

    define(ctx, "boolean?", scm_boolean_p, arity_exactly(1), 1, any_c);
    define(ctx, "not", scm_not, arity_exactly(1), 1, any_c);
    define(ctx, "eq?", scm_eq, arity_exactly(2), 2, any_c, any_c);
    define(ctx, "eqv?", scm_eqv, arity_exactly(2), 2, any_c, any_c);
    define(ctx, "equal?", scm_equal, arity_exactly(2), 2, any_c, any_c);
}

