#include "scm.h"

////////////////////////////////////////////////////////////////////////////////
// globals
////////////////////////////////////////////////////////////////////////////////
scmval scm_0;
scmval scm_pos_inf;
scmval scm_neg_inf;
scmval scm_nan;

////////////////////////////////////////////////////////////////////////////////
// forward declarations
////////////////////////////////////////////////////////////////////////////////
static int ncmp(scmval, scmval);
static scmval nadd(scmval, scmval);
static scmval nmul(scmval, scmval);
static scmval nsub(scmval, scmval);

////////////////////////////////////////////////////////////////////////////////
// constructors
////////////////////////////////////////////////////////////////////////////////
scmval scm_fix(fixnum i) {
    scmval v = make_val(SCM_TYPE_FIXNUM);
    v.i = i;
    return v;
}

scmval scm_flo(flonum d) {
    scmval v = make_val(SCM_TYPE_FLONUM);
    v.d = d;
    return v;
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_number_p(scmval x) {
    return scm_bool(is_fixnum(x) || is_flonum(x));
}

static scmval scm_real_p(scmval x) {
    return scm_bool(is_fixnum(x) || is_flonum(x));
}

static scmval scm_integer_p(scmval x) {
    return scm_bool(is_fixnum(x));
}

static scmval scm_exact_p(scmval x) {
    return scm_bool(is_fixnum(x));
}

static scmval scm_inexact_p(scmval x) {
    return scm_bool(is_flonum(x));
}

static scmval scm_exact_integer_p(scmval x) {
    return scm_bool(is_fixnum(x));
}

static scmval scm_finite_p(scmval x) {
    return scm_bool(!(is_nan(x) || is_pos_inf(x) || is_neg_inf(x)));
}

static scmval scm_infinite_p(scmval x) {
    return scm_bool(is_pos_inf(x) || is_neg_inf(x));
}

static scmval scm_nan_p(scmval x) {
    return scm_bool(is_nan(x));
}

static scmval scm_zero_p(scmval x) {
    check_arg("zero?", number_c, x);
    if(is_pos_inf(x))       return scm_false;
    else if(is_neg_inf(x))  return scm_false;
    else if(is_nan(x))      return scm_false;
    return scm_bool((is_fixnum(x) && c_fix(x) == 0) || (is_flonum(x) && c_flo(x) == 0.0));
}

static scmval scm_positive_p(scmval x) {
    check_arg("positive?", number_c, x);
    if(is_pos_inf(x))       return scm_true;
    else if(is_neg_inf(x))  return scm_false;
    else if(is_nan(x))      return scm_false;
    return scm_bool((is_fixnum(x) && c_fix(x) >= 0) || (is_flonum(x) && c_flo(x) >= 0.0));
}

static scmval scm_negative_p(scmval x) {
    check_arg("negative?", number_c, x);
    if(is_pos_inf(x))       return scm_false;
    else if(is_neg_inf(x))  return scm_true;
    else if(is_nan(x))      return scm_false;
    return scm_bool((is_fixnum(x) && c_fix(x) < 0) || (is_flonum(x) && c_flo(x) < 0.0));
}

#define make_number_comparator(NAME, CNAME, PRED)           \
    static inline scmval CNAME(int argc, scmval* argv) {    \
        check_args(NAME, number_c, argc, argv);             \
        scmval x, y;                                        \
        x = argv[0];                                        \
        for(int i = 1; i < argc; i++) {                     \
            y = argv[i];                                    \
            if(!(ncmp(x, y) PRED 0)) return scm_false;      \
            x = y;                                          \
        }                                                   \
        return scm_true;                                    \
    }

make_number_comparator("=",  scm_number_eq_p, ==)
make_number_comparator("<",  scm_number_lt_p, <)
make_number_comparator(">",  scm_number_gt_p, >)
make_number_comparator("<=", scm_number_le_p, <=)
make_number_comparator(">=", scm_number_ge_p, >=)

#undef make_number_comparator

static scmval scm_add(int argc, scmval* argv) {
    if(argc == 0) return scm_0;
    check_args("+", number_c, argc, argv);
    scmval x = argv[0];
    for(int i = 1; i < argc; i++) {
        x = nadd(x, argv[i]);
    }
    return x;
}

static scmval scm_mul(int argc, scmval* argv) {
    if(argc == 0) return scm_fix(1);
    check_args("*", number_c, argc, argv);
    scmval x = argv[0];
    for(int i = 1; i < argc; i++) {
        x = nmul(x, argv[i]);
    }
    return x;
}

static scmval scm_sub(int argc, scmval* argv) {
    check_args("-", number_c, argc, argv);
    if(argc == 1) return nsub(scm_0, argv[0]);
    scmval x = argv[0];
    for(int i = 1; i < argc; i++) {
        x = nsub(x, argv[i]);
    }
    return x;
}

static scmval scm_exact(scmval n) {
    check_arg("exact", number_c, n);
    if(is_fixnum(n))
        return n;
    return scm_fix((fixnum)c_flo(n));
}

static scmval scm_truncate(scmval n) {
    check_arg("truncate", number_c, n);
    if(is_fixnum(n))
        return n;
    flonum f = c_flo(n);
    return scm_flo(trunc(f));
}

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void init_number(scmval env) {
    define(env, "number?",           scm_number_p,           arity_exactly(1));
    define(env, "real?",             scm_real_p,             arity_exactly(1));
    define(env, "integer?",          scm_integer_p,          arity_exactly(1));
    define(env, "exact?",            scm_exact_p,            arity_exactly(1));
    define(env, "inexact?",          scm_inexact_p,          arity_exactly(1));
    define(env, "exact-integer?",    scm_exact_integer_p,    arity_exactly(1));
    define(env, "finite?",           scm_finite_p,           arity_exactly(1));
    define(env, "infinite?",         scm_infinite_p,         arity_exactly(1));
    define(env, "nan?",              scm_nan_p,              arity_exactly(1));
    define(env, "zero?",             scm_zero_p,             arity_exactly(1));
    define(env, "positive?",         scm_positive_p,         arity_exactly(1));
    define(env, "negative?",         scm_negative_p,         arity_exactly(1));
    define(env, "=",                 scm_number_eq_p,        arity_at_least(2));
    define(env, "<",                 scm_number_lt_p,        arity_at_least(2));
    define(env, ">",                 scm_number_gt_p,        arity_at_least(2));
    define(env, "<=",                scm_number_le_p,        arity_at_least(2));
    define(env, ">=",                scm_number_ge_p,        arity_at_least(2));
    define(env, "+",                 scm_add,                arity_at_least(0));
    define(env, "*",                 scm_mul,                arity_at_least(0));
    define(env, "-",                 scm_sub,                arity_at_least(1));
    define(env, "exact",             scm_exact,              arity_exactly(1));
    define(env, "truncate",          scm_truncate,           arity_exactly(1));

    scm_0       = scm_fix(0);
    scm_pos_inf = scm_flo(HUGE_VAL);
    scm_neg_inf = scm_flo(-HUGE_VAL);
    scm_nan     = scm_flo(NAN);
}

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////
bool numeq(scmval x, scmval y) {
    return ncmp(x,y) == 0;
}

static int ncmp(scmval x, scmval y) {
    int res = 0;
    if(is_fixnum(x)) {
        if(is_fixnum(y)) {
            fixnum diff = c_fix(x) - c_fix(y);
            if(diff < 0)        res = -1;
            else if(diff > 0)   res = 1;
            else                res = 0;
        } else if(is_flonum(y)) {
            flonum diff = c_fix(x) - c_flo(y);
            if(diff < 0)        res = -1;
            else if(diff > 0)   res = 1;
            else                res = 0;
        }
    } else if(is_flonum(x)) {
        if(is_fixnum(y)) {
            res = -ncmp(y, x);
        } else if(is_flonum(y)) {
            flonum diff = c_flo(x) - c_flo(y);
            if(diff < 0)        res = -1;
            else if(diff > 0)   res = 1;
            else                res = 0;
            
        }
    }
    return res;
}

static scmval nadd(scmval x, scmval y) {
    scmval r = scm_0;
    if(is_fixnum(x)) {
        if(is_fixnum(y)) {
            r = scm_fix(c_fix(x) + c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_fix(x) + c_flo(y));
        }
    } else if(is_flonum(x)) {
        if(is_fixnum(y)) {
            r = nadd(y, x);
        } else if(is_flonum(y)) {
            r = scm_flo(c_flo(x) + c_flo(y));
        }
    }
    return r;
}

static scmval nmul(scmval x, scmval y) {
    scmval r = scm_fix(1);
    if(is_fixnum(x)) {
        if(is_fixnum(y)) {
            r = scm_fix(c_fix(x) * c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_fix(x) * c_flo(y));
        }
    } else if(is_flonum(x)) {
        if(is_fixnum(y)) {
            r = nmul(y, x);
        } else if(is_flonum(y)) {
            r = scm_flo(c_flo(x) * c_flo(y));
        }
    }
    return r;
}

static scmval nsub(scmval x, scmval y) {
    scmval r = scm_0;
    if(is_fixnum(x)) {
        if(is_fixnum(y)) {
            r = scm_fix(c_fix(x) - c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_fix(x) - c_flo(y));
        }
    } else if(is_flonum(x)) {
        if(is_fixnum(y)) {
            r = scm_flo(c_flo(x) - c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_flo(x) - c_flo(y));
        }
    }
    return r;
}

