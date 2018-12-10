#include "scm.h"

// constructor
scmval make_char(scm_char_t c) {
    scmval v = make_val(SCM_TYPE_CHAR);
    v.c = c;
    return v;
}

// standard library
scmval scm_char_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_char(v))
        return scm_true;
    return scm_false;
}

#define make_char_comparator(CNAME, PRED, CI)       \
    static inline scmval CNAME(scm_ctx_t* ctx) {    \
        scm_char_t c, c1;                           \
        bool eq;                                    \
        int argc;                                   \
        scmval *argv;                               \
        arg_ref_list(ctx, &argc, &argv);            \
        c = char_value(argv[0]);                    \
        for(int i = 1; i < argc; i++) {             \
            c1 = char_value(argv[i]);               \
            eq = CI                                 \
                ? tolower(c) PRED tolower(c1)       \
                : c PRED c1;                        \
            if(!eq) return scm_false;               \
            c = c1;                                 \
        }                                           \
        return scm_true;                            \
    }

make_char_comparator(scm_char_eq_p, ==, false)
make_char_comparator(scm_char_lt_p, <,  false)
make_char_comparator(scm_char_gt_p, >,  false)
make_char_comparator(scm_char_le_p, <=, false)
make_char_comparator(scm_char_ge_p, >=, false)

make_char_comparator(scm_char_ci_eq_p, ==, true)
make_char_comparator(scm_char_ci_lt_p, <,  true)
make_char_comparator(scm_char_ci_gt_p, >,  true)
make_char_comparator(scm_char_ci_le_p, <=, true)
make_char_comparator(scm_char_ci_ge_p, >=, true)

#undef make_char_comparator

#define make_char_predicate(CNAME, PRED)            \
    static inline scmval CNAME(scm_ctx_t* ctx) {    \
        scmval v;                                   \
        v = arg_ref(ctx, 0);                        \
        if(PRED(char_value(v)))                     \
            return scm_true;                        \
        return scm_false;                           \
    }

make_char_predicate(scm_char_alphabetic_p, isalpha)
make_char_predicate(scm_char_numeric_p, isdigit)
make_char_predicate(scm_char_whitespace_p, isspace)
make_char_predicate(scm_char_upper_p, isupper)
make_char_predicate(scm_char_lower_p, islower)

#undef make_char_predicate

static inline scmval scm_char_digit_value(scm_ctx_t* ctx) {
    scm_char_t c;
    scmval v;
    v = arg_ref(ctx, 0);
    c = char_value(v);
    if(isdigit(c))
        return make_fixnum(c - '0');
    return scm_false;
}

static inline scmval scm_char_to_integer(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    return make_fixnum(char_value(v));
}

static inline scmval scm_integer_to_char(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    // FIXME check value to prevent overflow
    return make_char((scm_char_t)fixnum_value(v));
}

static inline scmval scm_char_upcase(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    return make_char(toupper(char_value(v)));
}

static inline scmval scm_char_downcase(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    return make_char(tolower(char_value(v)));
}

void init_char(scm_ctx_t* ctx) {
    define(ctx, "char?", scm_char_p, arity_exactly(1), 1, any_c);
    define(ctx, "char=?",  scm_char_eq_p, arity_at_least(2), 1, char_c);
    define(ctx, "char<?",  scm_char_lt_p, arity_at_least(2), 1, char_c);
    define(ctx, "char>?",  scm_char_gt_p, arity_at_least(2), 1, char_c);
    define(ctx, "char<=?", scm_char_le_p, arity_at_least(2), 1, char_c);
    define(ctx, "char>=?", scm_char_ge_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-ci=?",  scm_char_ci_eq_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-ci<?",  scm_char_ci_lt_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-ci>?",  scm_char_ci_gt_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-ci<=?", scm_char_ci_le_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-ci>=?", scm_char_ci_ge_p, arity_at_least(2), 1, char_c);
    define(ctx, "char-alphabetic?", scm_char_alphabetic_p, arity_exactly(1), 1, char_c);
    define(ctx, "char-numeric?", scm_char_numeric_p, arity_exactly(1), 1, char_c);
    define(ctx, "char-whitespace?", scm_char_whitespace_p, arity_exactly(1), 1, char_c);
    define(ctx, "char-upper-case?", scm_char_upper_p, arity_exactly(1), 1, char_c);
    define(ctx, "char-lower-case?", scm_char_lower_p, arity_exactly(1), 1, char_c);
    define(ctx, "digit-value", scm_char_digit_value, arity_exactly(1), 1, char_c);
    define(ctx, "char->integer", scm_char_to_integer, arity_exactly(1), 1, char_c);
    define(ctx, "integer->char", scm_integer_to_char, arity_exactly(1), 1, fixnum_c);
    define(ctx, "char-upcase", scm_char_upcase, arity_exactly(1), 1, char_c);
    define(ctx, "char-downcase", scm_char_downcase, arity_exactly(1), 1, char_c);

}

