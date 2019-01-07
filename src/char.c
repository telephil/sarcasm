#include "scm.h"

// constructor
scmval s_char(char c) {
    scmval v = make_val(SCM_TYPE_CHAR);
    v.c = c;
    return v;
}

// standard library
scmval scm_char_p(scmval v) {
    return s_bool(is_char(v));
}

#define make_char_comparator(NAME, CNAME, PRED, CI)             \
    static inline scmval CNAME(int argc, scmval* argv) {        \
        check_args(NAME, char_c, argc, argv);                   \
        char c, c1;                                             \
        bool eq;                                                \
        c = c_char(argv[0]);                                    \
        for(int i = 1; i < argc; i++) {                         \
            c1 = c_char(argv[i]);                               \
            eq = CI                                             \
                ? tolower(c) PRED tolower(c1)                   \
                : c PRED c1;                                    \
            if(!eq) return scm_false;                           \
            c = c1;                                             \
        }                                                       \
        return scm_true;                                        \
    }

make_char_comparator("char=?",  scm_char_eq_p, ==, false)
make_char_comparator("char<?",  scm_char_lt_p, <,  false)
make_char_comparator("char>?",  scm_char_gt_p, >,  false)
make_char_comparator("char<=?", scm_char_le_p, <=, false)
make_char_comparator("char>=?", scm_char_ge_p, >=, false)

make_char_comparator("char-ci=?",   scm_char_ci_eq_p, ==, true)
make_char_comparator("char-ci<?",   scm_char_ci_lt_p, <,  true)
make_char_comparator("char-ci>?",   scm_char_ci_gt_p, >,  true)
make_char_comparator("char-ci<=?",  scm_char_ci_le_p, <=, true)
make_char_comparator("char-ci>=?",  scm_char_ci_ge_p, >=, true)

#undef make_char_comparator

#define make_char_predicate(NAME, CNAME, PRED)  \
    static inline scmval CNAME(scmval v) {      \
        check_arg(NAME, char_c, v);             \
        return s_bool(PRED(c_char(v)));         \
    }

make_char_predicate("char-alphabetic?", scm_char_alphabetic_p,  isalpha)
make_char_predicate("char-numeric?",    scm_char_numeric_p,     isdigit)
make_char_predicate("char-whitespace?", scm_char_whitespace_p,  isspace)
make_char_predicate("char-upper-case?", scm_char_upper_p,       isupper)
make_char_predicate("char-lower-case?", scm_char_lower_p,       islower)

#undef make_char_predicate

static inline scmval scm_char_digit_value(scmval v) {
    check_arg("digit-value", char_c, v);
    char c = c_char(v);
    if(isdigit(c))
        return s_fix(c - '0');
    return scm_false;
}

static inline scmval scm_char_to_integer(scmval v) {
    check_arg("char->integer", char_c, v);
    return s_fix(c_char(v));
}

static inline scmval scm_integer_to_char(scmval v) {
    check_arg("integer->char", fixnum_c, v);
    // FIXME check value to prevent overflow
    return s_char((char)c_fix(v));
}

static inline scmval scm_char_upcase(scmval v) {
    check_arg("char-upcase", char_c, v);
    return s_char(toupper(c_char(v)));
}

static inline scmval scm_char_downcase(scmval v) {
    check_arg("char-downcase", char_c, v);
    return s_char(tolower(c_char(v)));
}

void init_char(scmval env) {
    define(env, "char?",            scm_char_p,             arity_exactly(1));
    define(env, "char=?",           scm_char_eq_p,          arity_at_least(2));
    define(env, "char<?",           scm_char_lt_p,          arity_at_least(2));
    define(env, "char>?",           scm_char_gt_p,          arity_at_least(2));
    define(env, "char<=?",          scm_char_le_p,          arity_at_least(2));
    define(env, "char>=?",          scm_char_ge_p,          arity_at_least(2));
    define(env, "char-ci=?",        scm_char_ci_eq_p,       arity_at_least(2));
    define(env, "char-ci<?",        scm_char_ci_lt_p,       arity_at_least(2));
    define(env, "char-ci>?",        scm_char_ci_gt_p,       arity_at_least(2));
    define(env, "char-ci<=?",       scm_char_ci_le_p,       arity_at_least(2));
    define(env, "char-ci>=?",       scm_char_ci_ge_p,       arity_at_least(2));
    define(env, "char-alphabetic?", scm_char_alphabetic_p,  arity_exactly(1));
    define(env, "char-numeric?",    scm_char_numeric_p,     arity_exactly(1));
    define(env, "char-whitespace?", scm_char_whitespace_p,  arity_exactly(1));
    define(env, "char-upper-case?", scm_char_upper_p,       arity_exactly(1));
    define(env, "char-lower-case?", scm_char_lower_p,       arity_exactly(1));
    define(env, "digit-value",      scm_char_digit_value,   arity_exactly(1));
    define(env, "char->integer",    scm_char_to_integer,    arity_exactly(1));
    define(env, "integer->char",    scm_integer_to_char,    arity_exactly(1));
    define(env, "char-upcase",      scm_char_upcase,        arity_exactly(1));
    define(env, "char-downcase",    scm_char_downcase,      arity_exactly(1));
}

