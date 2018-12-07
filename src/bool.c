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
    bool r = false;
    switch(type_of(x)) {
        case SCM_TYPE_NULL:
            r = true;
            break;
        case SCM_TYPE_BOOL:
            r = (get_bool(x) == get_bool(y));
            break;
        case SCM_TYPE_FIXNUM:
            r = (get_fixnum(x) == get_fixnum(y));
            break;
        case SCM_TYPE_FLONUM:
            r = (get_flonum(x) == get_flonum(y));
            break;
        case SCM_TYPE_CHAR:
            r = (get_char(x) == get_char(y));
            break;
        default:
            r = (x.o == y.o);
            break;
    }
    return r;
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
    if(is_eqv(x, y))
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

// initialization
void init_bool(scm_ctx_t* ctx) {
    scm_true  = make_bool(true);
    scm_false = make_bool(false);

    define(ctx, "boolean?", scm_boolean_p, arity_exactly(1), 1, any_c);
    define(ctx, "not", scm_not, arity_exactly(1), 1, any_c);
    define(ctx, "eq?", scm_eq, arity_exactly(2), 2, any_c, any_c);
    define(ctx, "eqv?", scm_eqv, arity_exactly(2), 2, any_c, any_c);
}

