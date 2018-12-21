#include <gc/cord_pos.h>
#include "scm.h"

// constructor
scmval make_string(const char* s) {
    scm_string_t* c = scm_new(scm_string_t);
    c->value = CORD_from_char_star(s);
    return make_ptr(SCM_TYPE_STRING, c);
}

scmval make_string_from_cord(CORD c) {
    scm_string_t* s = scm_new(scm_string_t);
    s->value = c;
    return make_ptr(SCM_TYPE_STRING, s);
}

// standard library
static scmval scm_string_p(scmval v) {
    return scm_bool(is_string(v));
}

static scmval scm_make_string(scmval k, scmval c) {
    opt_arg(c, make_char(' '));
    check_arg("make-string", fixnum_c, k);
    check_arg("make-string", char_c, c);
    return make_string_from_cord(CORD_chars(char_value(c), fixnum_value(k)));
}

static scmval scm_string(scmfix argc, scmval* argv) {
    check_args("string", char_c, argc, argv);
    char* s = scm_new_array(argc+1, char);
    for(int i = 0; i < argc; i++) {
        s[i] = char_value(argv[i]);
    }
    s[argc] = '\0';
    return make_string(s);
}

static scmval scm_string_length(scmval s) {
    check_arg("string-length", string_c, s);
    return make_fixnum(string_length(s));
}

static scmval scm_string_ref(scmval s, scmval k) {
    check_arg("string-ref", string_c, s);
    check_arg("string-ref", fixnum_c, k);
    check_range("string-ref", fixnum_value(k), 0, string_length(s));
    char c = CORD_fetch(string_value(s), fixnum_value(k));
    return make_char(c);
}

static scmval scm_string_set(scmval s, scmval k, scmval c) {
    check_arg("string-set!", string_c, s);
    check_arg("string-set!", fixnum_c, k);
    check_arg("string-set!", char_c,   c);
    check_range("string-set!", fixnum_value(k), 0, string_length(s));
    char* ns = string_to_cstr(s);
    ns[fixnum_value(k)] = char_value(c);
    get_string(s)->value = CORD_from_char_star(ns);
    return scm_undef;
}

static int CORD_cmpi(CORD x, CORD y) {
    CORD_pos xp, yp;
    CORD_set_pos(xp, x, 0);
    CORD_set_pos(yp, y, 0);
    while(true) {
        char xc = tolower(CORD_pos_fetch(xp));
        char yc = tolower(CORD_pos_fetch(yp));
        if(xc != yc)
            return (xc - yc);
        CORD_next(xp);
        CORD_next(yp);
        if(!CORD_pos_valid(xp)) // reached end of x
            return !CORD_pos_valid(yp) ? 0 : -1;
        if(!CORD_pos_valid(yp)) // reached end of y
            return 1;
    }
    return 0; // never reached
}

#define make_string_comparator(NAME, CNAME, PRED, CMP)  \
    static scmval CNAME(scmfix argc, scmval* argv) {    \
        check_args(NAME, string_c, argc, argv);         \
        CORD r, r1;                                     \
        r = string_value(argv[0]);                      \
        for(int i = 1; i < argc; i++) {                 \
            r1 = string_value(argv[i]);                 \
            if(!(CMP(r,r1) PRED 0)) return scm_false;   \
            r = r1;                                     \
        }                                               \
        return scm_true;                                \
    }

make_string_comparator("string=?",      scm_string_eq_p,    ==, CORD_cmp)
make_string_comparator("string<?",      scm_string_lt_p,    <,  CORD_cmp)
make_string_comparator("string>?",      scm_string_gt_p,    >,  CORD_cmp)
make_string_comparator("string<=?",     scm_string_le_p,    <=, CORD_cmp)
make_string_comparator("string>=?",     scm_string_ge_p,    >=, CORD_cmp)
make_string_comparator("string-ci=?",   scm_string_ci_eq_p, ==, CORD_cmpi)
make_string_comparator("string-ci<?",   scm_string_ci_lt_p, <,  CORD_cmpi)
make_string_comparator("string-ci>?",   scm_string_ci_gt_p, >,  CORD_cmpi)
make_string_comparator("string-ci<=?",  scm_string_ci_le_p, <=, CORD_cmpi)
make_string_comparator("string-ci>=?",  scm_string_ci_ge_p, >=, CORD_cmpi)

#undef make_string_comparator

static scmval scm_string_upcase(scmval s) {
    check_arg("string-upcase", string_c, s);
    char *r = string_to_cstr(s);
    for(char* p = r; (*p = toupper(*p)); p++) {}
    return make_string(r);
}

static scmval scm_string_downcase(scmval s) {
    check_arg("string-downcase", string_c, s);
    char *r = string_to_cstr(s);
    for(char* p = r; (*p = tolower(*p)); p++) {}
    return make_string(r);
}

static scmval scm_string_append(scmfix argc, scmval* argv) {
    check_args("string-append", string_c, argc, argv);
    CORD r = CORD_EMPTY;
    for(int i = 0; i < argc; i++) {
        r = CORD_cat(r, string_value(argv[i]));
    }
    return make_string_from_cord(r);
}

static scmval scm_string_to_list(scmval s, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end,   make_fixnum(string_length(s)-1));
    check_arg("string->list", string_c, s);
    check_arg("string->list", fixnum_c, start);
    check_arg("string->list", fixnum_c, end);
    check_range("string->list", fixnum_value(start), 0, string_length(s));
    check_range("string->list", fixnum_value(end), fixnum_value(start), string_length(s));
    scmval h, l = scm_null;
    CORD_pos pos;
    CORD_set_pos(pos, string_value(s), fixnum_value(start));
    for(int i = 0; i < (fixnum_value(end) - fixnum_value(start) + 1); i++) {
        char c = CORD_pos_fetch(pos);
        CORD_next(pos);
        scmval v = cons(make_char(c), scm_null);
        if(is_null(l)) {
            h = l = v;
        } else {
            setcdr(l, v);
            l = v;
        }
    }
    return h;
}

static scmval scm_list_to_string(scmval list) {
    check_arg("list->string", list_c, list);
    CORD r = CORD_EMPTY;
    for(scmval l = list; !is_null(l); l = cdr(l)) {
        if(!is_char(car(l)))
            type_error("list->string", char_c, car(l));
        r = CORD_cat(r, CORD_chars(char_value(car(l)), 1));
    }
    return make_string_from_cord(r);
}

static scmval scm_string_fill(scmval s, scmval fill, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end,   make_fixnum(string_length(end)));
    check_arg("string-fill!", string_c, s);
    check_arg("string-fill!", char_c, fill);
    check_arg("string-fill!", fixnum_c, start);
    check_arg("string-fill!", fixnum_c, end);
    check_range("string-fill!", fixnum_value(start), 0, string_length(s));
    check_range("string-fill!", fixnum_value(end), fixnum_value(start), string_length(s));
    char* ns = string_to_cstr(s);
    for(int i = fixnum_value(start); i <= fixnum_value(end); i++) {
        ns[i] = char_value(fill);
    }
    get_string(s)->value = CORD_from_char_star(ns);
    return scm_undef;
}

static scmval string_copy(const char* name, scmval s, scmval start, scmval end) {
    check_arg(name, string_c, s);
    check_arg(name, fixnum_c, start);
    check_arg(name, fixnum_c, end);
    check_range(name, fixnum_value(start), 0, string_length(s));
    check_range(name, fixnum_value(end), fixnum_value(start),  string_length(s));
    CORD r = CORD_substr(string_value(s), fixnum_value(start), fixnum_value(end) - fixnum_value(start) + 1);
    return make_string_from_cord(r);
}

static scmval scm_substring(scmval s, scmval start, scmval end) {
    return string_copy("substring", s, start, end); 
}

static scmval scm_string_copy(scmval s, scmval start, scmval end) {
    return string_copy("string-copy", s, start, end);
}

static scmval scm_string_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end,   make_fixnum(string_length(from)-1));
    check_arg("string-copy!", string_c, to);
    check_arg("string-copy!", fixnum_c, at);
    check_arg("string-copy!", string_c, from);
    check_arg("string-copy!", fixnum_c, start);
    check_arg("string-copy!", fixnum_c, end);
    check_range("string-copy!", fixnum_value(at), 0, string_length(to));
    check_range("string-copy!", fixnum_value(start), 0, string_length(from));
    check_range("string-copy!", fixnum_value(end), fixnum_value(start), string_length(from));
    char* sto = string_to_cstr(to);
    char* sfrom = string_to_cstr(from);
    strncpy(sto+fixnum_value(at), sfrom+fixnum_value(start), fixnum_value(end)-fixnum_value(start)+1);
    get_string(to)->value = CORD_from_char_star(sto);
    return scm_undef;
}


void init_string() {
    define("string?",           scm_string_p,           arity_exactly(1));
    define("make-string",       scm_make_string,        arity_or(1, 2));
    define("string",            scm_string,             arity_at_least(1));
    define("string-length",     scm_string_length,      arity_exactly(1));
    define("string-ref",        scm_string_ref,         arity_exactly(2));
    define("string-set!",       scm_string_set,         arity_exactly(3));
    define("string=?",          scm_string_eq_p,        arity_at_least(2));
    define("string<?",          scm_string_lt_p,        arity_at_least(2));
    define("string>?",          scm_string_gt_p,        arity_at_least(2));
    define("string<=?",         scm_string_le_p,        arity_at_least(2));
    define("string>=?",         scm_string_ge_p,        arity_at_least(2));
    define("string-ci=?",       scm_string_ci_eq_p,     arity_at_least(2));
    define("string-ci<?",       scm_string_ci_lt_p,     arity_at_least(2));
    define("string-ci>?",       scm_string_ci_gt_p,     arity_at_least(2));
    define("string-ci<=?",      scm_string_ci_le_p,     arity_at_least(2));
    define("string-ci>=?",      scm_string_ci_ge_p,     arity_at_least(2));
    define("string-upcase",     scm_string_upcase,      arity_exactly(1));
    define("string-downcase",   scm_string_downcase,    arity_exactly(1));
    define("substring",         scm_substring,          arity_exactly(3));
    define("string-append",     scm_string_append,      arity_at_least(2));
    define("string->list",      scm_string_to_list,     arity_between(1, 3));
    define("list->string",      scm_list_to_string,     arity_exactly(1));
    define("string-copy",       scm_string_copy,        arity_between(1, 3));
    define("string-copy!",      scm_string_mcopy,       arity_between(3, 5));
    define("string-fill!",      scm_string_fill,        arity_between(2, 4));
}

