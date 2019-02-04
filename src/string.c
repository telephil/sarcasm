#include "scm.h"

// constructor
scmval s_str(const char* s) {
    scm_string_t* c = scm_gc_malloc(sizeof(scm_string_t));
    c->value = scm_gc_strdup(s);
    return make_ptr(SCM_TYPE_STRING, c);
}

scmval s_str_nocopy(char* s) {
    scm_string_t* c = scm_gc_malloc(sizeof(scm_string_t));
    c->value = s;
    return make_ptr(SCM_TYPE_STRING, c);
}

// standard library
static scmval scm_string_p(scmval v) {
    return s_bool(is_string(v));
}

static scmval scm_make_string(scmval k, scmval c) {
    opt_arg(c, s_char(' '));
    check_arg("make-string", fixnum_c, k);
    check_arg("make-string", char_c, c);
    char* s = scm_gc_malloc_atomic((c_fix(k) + 1) * sizeof(char));
    for(int i = 0; i < c_fix(k); i++) {
        s[i] = c_char(c);
    }
    s[c_fix(k)] = '\0';
    return s_str_nocopy(s);
}

static scmval scm_string(int argc, scmval* argv) {
    check_args("string", char_c, argc, argv);
    char* s = scm_gc_malloc_atomic((argc+1) * sizeof(char));
    for(int i = 0; i < argc; i++) {
        s[i] = c_char(argv[i]);
    }
    s[argc] = '\0';
    return s_str_nocopy(s);
}

static scmval scm_string_length(scmval s) {
    check_arg("string-length", string_c, s);
    return s_fix(string_length(s));
}

static scmval scm_string_ref(scmval s, scmval k) {
    check_arg("string-ref", string_c, s);
    check_arg("string-ref", fixnum_c, k);
    check_range("string-ref", c_fix(k), 0, string_length(s));
    char c = string_ref(s, c_fix(k));
    return s_char(c);
}

static scmval scm_string_set(scmval s, scmval k, scmval c) {
    check_arg("string-set!", string_c, s);
    check_arg("string-set!", fixnum_c, k);
    check_arg("string-set!", char_c,   c);
    check_range("string-set!", c_fix(k), 0, string_length(s));
    string_set(s, c_fix(k), c_char(c));
    return scm_void;
}

static int strcmpi(char* x, char* y) {
    int xi = 0, yi = 0;
    size_t xl = strlen(x), yl = strlen(y);
    while(true) {
        char xc = tolower(x[xi]);
        char yc = tolower(y[yi]);
        if(xc != yc)
            return (xc - yc);
        ++xi;
        ++yi;
        if(xi >= xl) // reached end of x
            return yi >= yl ? 0 : -1;
        if(yi >= yl) // reached end of y
            return 1;
    }
    return 0; // never reached
}

#define make_string_comparator(NAME, CNAME, PRED, CMP)  \
    static scmval CNAME(int argc, scmval* argv) {       \
        check_args(NAME, string_c, argc, argv);         \
        char *r, *r1;                                   \
        r = c_str(argv[0]);                             \
        for(int i = 1; i < argc; i++) {                 \
            r1 = c_str(argv[i]);                        \
            if(!(CMP(r,r1) PRED 0)) return scm_false;   \
            r = r1;                                     \
        }                                               \
        return scm_true;                                \
    }

make_string_comparator("string=?",      scm_string_eq_p,    ==, strcmp)
make_string_comparator("string<?",      scm_string_lt_p,    <,  strcmp)
make_string_comparator("string>?",      scm_string_gt_p,    >,  strcmp)
make_string_comparator("string<=?",     scm_string_le_p,    <=, strcmp)
make_string_comparator("string>=?",     scm_string_ge_p,    >=, strcmp)
make_string_comparator("string-ci=?",   scm_string_ci_eq_p, ==, strcmpi)
make_string_comparator("string-ci<?",   scm_string_ci_lt_p, <,  strcmpi)
make_string_comparator("string-ci>?",   scm_string_ci_gt_p, >,  strcmpi)
make_string_comparator("string-ci<=?",  scm_string_ci_le_p, <=, strcmpi)
make_string_comparator("string-ci>=?",  scm_string_ci_ge_p, >=, strcmpi)

#undef scm_str_comparator

static scmval scm_string_upcase(scmval s) {
    check_arg("string-upcase", string_c, s);
    char *r = c_str(s);
    for(char* p = r; (*p = toupper(*p)); p++) {}
    return s_str(r);
}

static scmval scm_string_downcase(scmval s) {
    check_arg("string-downcase", string_c, s);
    char *r = c_str(s);
    for(char* p = r; (*p = tolower(*p)); p++) {}
    return s_str(r);
}

static scmval scm_string_append(int argc, scmval* argv) {
    check_args("string-append", string_c, argc, argv);
    size_t s = 0;
    for(int i = 0; i < argc; i++)
        s += string_length(argv[i]);
    char* r = scm_gc_malloc_atomic((s+1)*sizeof(char));
    int offset = 0;
    for(int i = 0; i < argc; i++) {
        int l = string_length(argv[i]);
        memcpy(r + offset, c_str(argv[i]), l * sizeof(char));
        offset += l;
    }
    r[s] = '\0';
    return s_str_nocopy(r);
}

static scmval scm_string_to_list(scmval s, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(string_length(s)-1));
    check_arg("string->list", string_c, s);
    check_arg("string->list", fixnum_c, start);
    check_arg("string->list", fixnum_c, end);
    check_range("string->list", c_fix(start), 0, string_length(s));
    check_range("string->list", c_fix(end), c_fix(start), string_length(s));
    scmval l = scm_null;
    for(int i = strlen(c_str(s)) - 1; i >= 0; i++) {
        push(s_char(string_ref(s, i)), l);
    }
    return l;
}

static scmval scm_list_to_string(scmval list) {
    check_arg("list->string", list_c, list);
    size_t l = list_length(list);
    char* r = scm_gc_malloc_atomic((l+1)*sizeof(char));
    r[l] = '\0';
    int i = 0;
    foreach(elt, list) {
        if(!is_char(elt))
            type_error("list->string", char_c, elt);
        r[i++] = c_char(elt);
    }
    return s_str_nocopy(r);
}

static scmval scm_string_fill(scmval s, scmval fill, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(string_length(end)));
    check_arg("string-fill!", string_c, s);
    check_arg("string-fill!", char_c, fill);
    check_arg("string-fill!", fixnum_c, start);
    check_arg("string-fill!", fixnum_c, end);
    check_range("string-fill!", c_fix(start), 0, string_length(s));
    check_range("string-fill!", c_fix(end), c_fix(start), string_length(s));
    for(int i = c_fix(start); i <= c_fix(end); i++) {
        string_set(s, i, c_char(fill));
    }
    return scm_void;
}

static scmval string_copy(const char* name, scmval s, scmval start, scmval end) {
    check_arg(name, string_c, s);
    check_arg(name, fixnum_c, start);
    check_arg(name, fixnum_c, end);
    check_range(name, c_fix(start), 0, string_length(s));
    check_range(name, c_fix(end), c_fix(start),  string_length(s));
    int l = c_fix(end) - c_fix(start) + 1;
    char* r = scm_gc_malloc_atomic((l+1)*sizeof(char));
    r[l] = '\0';
    memcpy(r, c_str(s) + c_fix(start), l * sizeof(char));
    return s_str_nocopy(r);
}

static scmval scm_substring(scmval s, scmval start, scmval end) {
    return string_copy("substring", s, start, end); 
}

static scmval scm_string_copy(scmval s, scmval start, scmval end) {
    return string_copy("string-copy", s, start, end);
}

static scmval scm_string_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(string_length(from)-1));
    check_arg("string-copy!", string_c, to);
    check_arg("string-copy!", fixnum_c, at);
    check_arg("string-copy!", string_c, from);
    check_arg("string-copy!", fixnum_c, start);
    check_arg("string-copy!", fixnum_c, end);
    check_range("string-copy!", c_fix(at), 0, string_length(to));
    check_range("string-copy!", c_fix(start), 0, string_length(from));
    check_range("string-copy!", c_fix(end), c_fix(start), string_length(from));
    char* sto = c_str(to);
    char* sfrom = c_str(from);
    strncpy(sto+c_fix(at), sfrom+c_fix(start), c_fix(end)-c_fix(start)+1);
    return scm_void;
}

static inline void check_format_arg(int index, int argc, char clause) {
    if(index > (argc - 1))
        error(scm_undef, "no more arguments to match ~%c clause", clause);
}

static scmval scm_format(int argc, scmval* argv) {
    scmval v;
    int index = 0;
    int arg_index = 1;
    char* fmt = c_str(argv[0]);
    scmval port = scm_open_output_string();
    while(true) {
        char c = fmt[index++];
        if(c == '\0' || c == EOF)
            break;
        if(c != '~') {
            scm_putc(port, c);
            continue;
        }
        c = fmt[index++];
        switch(c) {
            case 'a':
                check_format_arg(arg_index, argc, 'a');
                scm_display(argv[arg_index++], port);
                break;
            case 'v':
                check_format_arg(arg_index, argc, 'v');
                scm_write(argv[arg_index++], port);
                break;
            case 'x':
                check_format_arg(arg_index, argc, 'x');
                v = argv[arg_index++];
                if(!is_number(v))
                    error(type_error_type, "clause ~x require a number argument but received %s", scm_to_cstr(v));
                v = number_to_string(v, s_fix(16));
                scm_display(v, port);
                break;
            case 'd':
                check_format_arg(arg_index, argc, 'd');
                v = argv[arg_index++];
                if(!is_number(v))
                    error(type_error_type, "clause ~d require a number argument but received %s", scm_to_cstr(v));
                v = number_to_string(v, s_fix(10));
                scm_display(v, port);
                break;
            case 'o':
                check_format_arg(arg_index, argc, 'o');
                v = argv[arg_index++];
                if(!is_number(v))
                    error(type_error_type, "clause ~o require a number argument but received %s", scm_to_cstr(v));
                v = number_to_string(v, s_fix(8));
                scm_display(v, port);
                break;
            case 'b':
                check_format_arg(arg_index, argc, 'b');
                v = argv[arg_index++];
                if(!is_number(v))
                    error(type_error_type, "clause ~b require a number argument but received %s", scm_to_cstr(v));
                v = number_to_string(v, s_fix(2));
                scm_display(v, port);
                break;
            case '%':
                scm_putc(port, '\n');
                break;
        }
    }
    return scm_get_output_string(port);
}

void init_string(scmval env) {
    define(env, "string?",           scm_string_p,           arity_exactly(1));
    define(env, "make-string",       scm_make_string,        arity_or(1, 2));
    define(env, "string",            scm_string,             arity_at_least(1));
    define(env, "string-length",     scm_string_length,      arity_exactly(1));
    define(env, "string-ref",        scm_string_ref,         arity_exactly(2));
    define(env, "string-set!",       scm_string_set,         arity_exactly(3));
    define(env, "string=?",          scm_string_eq_p,        arity_at_least(2));
    define(env, "string<?",          scm_string_lt_p,        arity_at_least(2));
    define(env, "string>?",          scm_string_gt_p,        arity_at_least(2));
    define(env, "string<=?",         scm_string_le_p,        arity_at_least(2));
    define(env, "string>=?",         scm_string_ge_p,        arity_at_least(2));
    define(env, "string-ci=?",       scm_string_ci_eq_p,     arity_at_least(2));
    define(env, "string-ci<?",       scm_string_ci_lt_p,     arity_at_least(2));
    define(env, "string-ci>?",       scm_string_ci_gt_p,     arity_at_least(2));
    define(env, "string-ci<=?",      scm_string_ci_le_p,     arity_at_least(2));
    define(env, "string-ci>=?",      scm_string_ci_ge_p,     arity_at_least(2));
    define(env, "string-upcase",     scm_string_upcase,      arity_exactly(1));
    define(env, "string-downcase",   scm_string_downcase,    arity_exactly(1));
    define(env, "substring",         scm_substring,          arity_exactly(3));
    define(env, "string-append",     scm_string_append,      arity_at_least(2));
    define(env, "string->list",      scm_string_to_list,     arity_between(1, 3));
    define(env, "list->string",      scm_list_to_string,     arity_exactly(1));
    define(env, "string-copy",       scm_string_copy,        arity_between(1, 3));
    define(env, "string-copy!",      scm_string_mcopy,       arity_between(3, 5));
    define(env, "string-fill!",      scm_string_fill,        arity_between(2, 4));
    define(env, "format",            scm_format,             arity_at_least(1));
}

