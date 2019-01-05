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
static scmval ndiv(scmval, scmval);

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

static scmval scm_odd_p(scmval x) {
    check_arg("odd?", number_c, x);
    if(!is_fixnum(x))
        return scm_true;
    return scm_bool(c_fix(x) & 1);
}

static scmval scm_even_p(scmval x) {
    check_arg("even?", number_c, x);
    return scm_not(scm_odd_p(x));
}

static scmval scm_max(int argc, scmval* argv) {
    check_args("max", number_c, argc, argv);
    scmval m = argv[0];
    for(int i = 1; i < argc; i++) {
        if(ncmp(m, argv[i]) < 0)
            m = argv[i];
    }
    return m;
}

static scmval scm_min(int argc, scmval* argv) {
    check_args("min", number_c, argc, argv);
    scmval m = argv[0];
    for(int i = 1; i < argc; i++) {
        if(ncmp(m, argv[i]) > 0)
            m = argv[i];
    }
    return m;
}

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

static scmval scm_div(int argc, scmval* argv) {
    check_args("/", number_c, argc, argv);
    if(argc == 1) return ndiv(scm_fix(1), argv[0]);
    scmval x = argv[0];
    for(int i = 1; i < argc; i++) {
        x = ndiv(x, argv[i]);
    }
    return x;
}

static scmval scm_abs(scmval x) {
    check_arg("abs", number_c, x);
    if(is_pos_inf(x))       return x;
    else if(is_neg_inf(x))  return scm_pos_inf;
    else if(is_nan(x))      return scm_nan;
    else if(is_fixnum(x)) {
        fixnum i = c_fix(x);
        if(i >= 0) return x;
        return scm_fix(-i);
    } else {
        flonum f = c_flo(x);
        if(f >= 0) return x;
        return scm_flo(-f);
    }
    return scm_undef; // not reached
}

static scmval scm_quotient(scmval n1, scmval n2) {
    check_arg("quotient", integer_c, n1);
    check_arg("quotient", integer_c, n2);
    fixnum i1 = c_fix(n1);
    fixnum i2 = c_fix(n2);
    ldiv_t d = ldiv(i1, i2);
    return scm_fix(d.quot);
}

static scmval scm_remainder(scmval n1, scmval n2) {
    check_arg("remainder", integer_c, n1);
    check_arg("remainder", integer_c, n2);
    fixnum i1 = c_fix(n1);
    fixnum i2 = c_fix(n2);
    ldiv_t d = ldiv(i1, i2);
    return scm_fix(d.rem);
}

static scmval scm_modulo(scmval n1, scmval n2) {
    check_arg("remainder", integer_c, n1);
    check_arg("remainder", integer_c, n2);
    fixnum i1 = c_fix(n1);
    fixnum i2 = c_fix(n2);
    fixnum m  = i1 % i2;
    if(m < 0)
        m = (i2 < 0) ? m - i2 : m + i2;
    return scm_fix(m);
}

static scmval scm_numerator(scmval q) {
    check_arg("numerator", number_c, q);
    return q;
}

static scmval scm_denominator(scmval q) {
    check_arg("denominator", number_c, q);
    return scm_fix(1);
}

static scmval scm_floor(scmval x) {
    check_arg("floor", number_c, x);
    if(is_pos_inf(x) || is_neg_inf(x) || is_nan(x))
        return x;
    if(is_integer(x))
        return x;
    return scm_flo(floor(c_flo(x)));
}

static scmval scm_ceiling(scmval x) {
    check_arg("ceiling", number_c, x);
    if(is_pos_inf(x) || is_neg_inf(x) || is_nan(x))
        return x;
    if(is_integer(x))
        return x;
    return scm_flo(ceil(c_flo(x)));
}

static scmval scm_truncate(scmval x) {
    check_arg("truncate", number_c, x);
    if(is_pos_inf(x) || is_neg_inf(x) || is_nan(x))
        return x;
    if(is_fixnum(x))
        return x;
    flonum f = c_flo(x);
    return scm_flo(trunc(f));
}

static scmval scm_round(scmval x) {
    check_arg("round", number_c, x);
    if(is_pos_inf(x) || is_neg_inf(x) || is_nan(x))
        return x;
    if(is_fixnum(x))
        return x;
    return scm_flo(round(c_flo(x)));
}

static inline flonum inexact(scmval z) { return is_fixnum(z) ? (flonum)c_fix(z) : c_flo(z); }

static scmval scm_exp(scmval z) {
    check_arg("exp", number_c, z);
    flonum f = inexact(z);
    return scm_flo(exp(f));
}

static scmval scm_log(scmval z1, scmval z2) {
    check_arg("log", number_c, z1);
    flonum res = 0.0;
    flonum f1 = inexact(z1);
    if(is_undef(z2)) {
        res = log(f1);
    } else {
        check_arg("log", number_c, z2);
        flonum f2 = inexact(z2);
        res = log(f1) / log(f2);
    }
    return scm_flo(res);
}

#define make_trig_function(CNAME, FNAME, CFUNC)     \
    static scmval CNAME(scmval z) {                 \
        check_arg(FNAME, number_c, z);              \
        flonum f = inexact(z);                      \
        return scm_flo(CFUNC(f));                   \
    }

make_trig_function(scm_cos,  "cos", cos)
make_trig_function(scm_sin,  "sin", sin)
make_trig_function(scm_tan,  "tan", tan)
make_trig_function(scm_acos, "acos", acos)
make_trig_function(scm_asin, "asin", asin)

#undef make_trig_function

static scmval scm_atan(scmval z1, scmval z2) {
    check_arg("atan", number_c, z1);
    flonum res;
    flonum f1 = inexact(z1);
    if(is_undef(z2)) {
        res = atan(f1);
    } else {
        check_arg("atan", number_c, z2);
        flonum f2 = inexact(z2);
        res = atan2(f1, f2);
    }
    return scm_flo(res);
}

static scmval scm_square(scmval z) {
    check_arg("square", number_c, z);
    return nmul(z, z);
}

static scmval scm_sqrt(scmval z) {
    check_arg("sqrt", number_c, z);
    if(ncmp(z, scm_0) < 0) error(scm_undef, "sqrt is not defined for negative numbers");
    scmval res;
    if(is_fixnum(z))
        res = scm_flo(sqrt(c_fix(z)));
    else
        res = scm_flo(sqrt(c_flo(z)));
    return res;
}

static scmval scm_expt(scmval z1, scmval z2) {
    check_arg("expt", number_c, z1);
    check_arg("expt", number_c, z2);
    flonum f1 = inexact(z1);
    flonum f2 = inexact(z2);
    flonum f3 = pow(f1, f2);
    if(is_fixnum(z1) && is_fixnum(z2))
        return scm_fix(f3);
    return scm_flo(f3);
}

static scmval scm_inexact(scmval x) {
    check_arg("inexact", number_c, x);
    if(is_flonum(x))
        return x;
    return scm_flo((flonum)c_fix(x));
}

static scmval scm_exact(scmval n) {
    check_arg("exact", number_c, n);
    if(is_fixnum(n))
        return n;
    return scm_fix((fixnum)c_flo(n));
}

static char* number_to_binary(fixnum z, char *result) {
  if(z > 1) {
    result = number_to_binary(z>>1, result);
  }
  *result = z & 1 ? '1' : '0';
  return result + 1;
}

static scmval scm_number_to_string(scmval z, scmval radix) {
    opt_arg(radix, scm_fix(10));
    check_arg("number->string", number_c, z);
    check_arg("number->string", radix_c, radix);
    if(is_flonum(z))
        return scm_to_string(z);
    scmval result;
    char *buf;
    fixnum i = c_fix(z);
    fixnum r = c_fix(radix);
    if(r == 2) {
        buf = scm_new_atomic(512, char);
        number_to_binary(i, buf);
    } else {
        char *fmt;
        switch(r) {
            case 8:  fmt = "%o"; break;
            case 10: fmt = "%ld";  break;
            case 16: fmt = "%x"; break;
        }
        asprintf(&buf, fmt, i);
        result = scm_str(buf);
    }
    return scm_str(buf);
}

static scmval scm_string_to_number(scmval s) {
    check_arg("string->number", string_c, s);
    return string_to_number(c_cstr(s));
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
    define(env, "odd?",              scm_odd_p,              arity_exactly(1));
    define(env, "even?",             scm_even_p,             arity_exactly(1));
    define(env, "max",               scm_max,                arity_at_least(2));
    define(env, "min",               scm_min,                arity_at_least(2));
    define(env, "=",                 scm_number_eq_p,        arity_at_least(2));
    define(env, "<",                 scm_number_lt_p,        arity_at_least(2));
    define(env, ">",                 scm_number_gt_p,        arity_at_least(2));
    define(env, "<=",                scm_number_le_p,        arity_at_least(2));
    define(env, ">=",                scm_number_ge_p,        arity_at_least(2));
    define(env, "+",                 scm_add,                arity_at_least(0));
    define(env, "*",                 scm_mul,                arity_at_least(0));
    define(env, "-",                 scm_sub,                arity_at_least(1));
    define(env, "/",                 scm_div,                arity_at_least(1));
    define(env, "abs",               scm_abs,                arity_exactly(1));
    define(env, "quotient",          scm_quotient,           arity_exactly(2));
    define(env, "remainder",         scm_remainder,          arity_exactly(2));
    define(env, "modulo",            scm_modulo,             arity_exactly(2));
    define(env, "numerator",         scm_numerator,          arity_exactly(1));
    define(env, "denominator",       scm_denominator,        arity_exactly(1));
    define(env, "floor",             scm_floor,              arity_exactly(1));
    define(env, "ceiling",           scm_ceiling,            arity_exactly(1));
    define(env, "truncate",          scm_truncate,           arity_exactly(1));
    define(env, "round",             scm_round,              arity_exactly(1));
    define(env, "exp",               scm_exp,                arity_exactly(1));
    define(env, "log",               scm_log,                arity_or(1, 2));
    define(env, "cos",               scm_cos,                arity_exactly(1));
    define(env, "sin",               scm_sin,                arity_exactly(1));
    define(env, "tan",               scm_tan,                arity_exactly(1));
    define(env, "acos",              scm_acos,               arity_exactly(1));
    define(env, "asin",              scm_asin,               arity_exactly(1));
    define(env, "atan",              scm_atan,               arity_or(1, 2));
    define(env, "square",            scm_square,             arity_exactly(1));
    define(env, "sqrt",              scm_sqrt,               arity_exactly(1));
    define(env, "expt",              scm_expt,               arity_exactly(2));
    define(env, "inexact",           scm_inexact,            arity_exactly(1));
    define(env, "exact",             scm_exact,              arity_exactly(1));
    define(env, "string->number",    scm_string_to_number,   arity_exactly(1));
    define(env, "number->string",    scm_number_to_string,   arity_or(1, 2));

    scm_0       = scm_fix(0);
    scm_pos_inf = scm_flo(HUGE_VAL);
    scm_neg_inf = scm_flo(-HUGE_VAL);
    scm_nan     = scm_flo(NAN);
}

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////
static bool is_valid_digit(char c, int base);

scmval string_to_number(char* buf) {
    int base = 10;
    bool dot = false;
    bool neg = false;
    bool is_int = false;
    char *p = buf, *q;

    // constants
    if(strncmp(buf, "+nan.0", 6) == 0)
        return scm_nan;
    else if(strncmp(buf, "+inf.0", 6) == 0)
        return scm_pos_inf;
    else if(strncmp(buf, "-inf.0", 6) == 0)
        return scm_neg_inf;

    // base
    if(*p == '#') {
        p++;
        switch(*p) {
            case 'b': base =  2; break;
            case 'o': base =  8; break;
            case 'd': base = 10; break;
            case 'x': base = 16; break;
            default: 
              return scm_false;
        }
        p++;
    } 
    if(*p == '+' || *p == '-') {
        if(*p == '-')
            neg = true;
        p++;
    }
    if(!*p)
        return scm_false;
    is_int = true;
    for(q = p; *q; q++) {
        if(*q == '.') {
            if(dot) return scm_false; // already found a dot
            if(base != 10) return scm_false;
            dot = true;
            is_int = false;
        } else if(!is_valid_digit(*q, base))
            return scm_false;
    }

    if(is_int) {
        fixnum l = strtol(p, NULL, base);
        if(neg) l = -l;
        return scm_fix(l);
    } else {
        flonum f = strtod(p, NULL);
        if(neg) f = -f;
        return scm_flo(f);
    }

    return scm_false;
}

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

static scmval ndiv(scmval x, scmval y) {
    scmval r = scm_fix(1);
    if(is_fixnum(x)) {
        if(is_fixnum(y)) {
            r = scm_fix(c_fix(x) / c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_fix(x) / c_flo(y));
        }
    } else if(is_flonum(x)) {
        if(is_fixnum(y)) {
            r = scm_flo(c_flo(x) / c_fix(y));
        } else if(is_flonum(y)) {
            r = scm_flo(c_flo(x) / c_flo(y));
        }
    }
    return r;
}

static bool is_valid_digit(char c, int base) {
    bool valid = false;
    switch(base) {
        case 2:
            valid = (c == '0' || c == '1');
            break;
        case 8:
            valid = (c >= '0' && c <= '7');
            break;
        case 10:
            valid = (c >= '0' && c <= '9');
            break;
        case 16:
            valid = (c >= '0' && c <= '9')
                 || (c >= 'a' && c <= 'f')
                 || (c >= 'A' && c <= 'F');
            break;
    }
    return valid;
}

