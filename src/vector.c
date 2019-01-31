#include "scm.h"

// constructor
scmval make_vector(size_t size, scmval initial) {
    scm_vector_t* v = scm_new(scm_vector_t);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    for(int i = 0; i < size; i++) {
        v->elts[i] = initial;
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

scmval make_vector_from_list(int size, scmval l) {
    int i;
    scmval c;
    scm_vector_t* v = scm_new(scm_vector_t);
    if(size < 0)
        size = list_length(l);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    for(c = l, i = 0; !is_null(c); c = cdr(c), i++) {
        v->elts[i] = car(c);
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

// standard library
static scmval scm_vector_p(scmval v) {
    return s_bool(is_vector(v));
}

static scmval scm_make_vector(scmval s, scmval i) {
    check_arg("make-vector", fixnum_c, s);
    opt_arg(i, scm_0);
    return make_vector(c_fix(s), i);
}

static scmval scm_vector(int argc, scmval* argv) {
    scmval r;
    r = make_vector(argc, scm_undef);
    for(int i = 0; i < argc; i++) {
        vector_set(r, i, argv[i]);
    }
    return r;
}

static scmval scm_vector_length(scmval vec) {
    check_arg("vector-length", vector_c, vec);
    return s_fix(vector_size(vec));
}

static scmval scm_vector_ref(scmval v, scmval i) {
    check_arg("vector-ref", vector_c, v);
    check_arg("vector-ref", fixnum_c, i);
    check_range("vector-ref", c_fix(i), 0, vector_size(v));
    return vector_ref(v, c_fix(i));
}

static scmval scm_vector_set(scmval v, scmval i, scmval x) {
    check_arg("vector-set!", vector_c, v);
    check_arg("vector-set!", fixnum_c, i);
    check_range("vector-set!", c_fix(i), 0, vector_size(v));
    vector_set(v, c_fix(i), x);
    return scm_void;
}

static scmval scm_vector_to_list(scmval v, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(vector_size(v) - 1));
    check_arg("vector->list", vector_c, v);
    check_arg("vector->list", fixnum_c, start);
    check_arg("vector->list", fixnum_c, end);
    check_range("vector->list", c_fix(start), 0, vector_size(v));
    check_range("vector->list", c_fix(end), c_fix(start), vector_size(v));
    scmval r = scm_null;
    for(int i = c_fix(end); i >= c_fix(start); i--) {
        r = cons(vector_ref(v, i), r);
    }
    return r;
}

static scmval scm_list_to_vector(scmval l) {
    check_arg("list->vector", list_c, l);
    int s = list_length(l);
    scmval r = make_vector(s, scm_undef);
    for(int i = 0; i < s; i++, l = cdr(l)) {
        vector_set(r, i, car(l));
    }
    return r;
}

static scmval scm_vector_to_string(scmval v, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(vector_size(v) - 1));
    check_arg("vector->string", vector_c, v);
    check_arg("vector->string", fixnum_c, start);
    check_arg("vector->string", fixnum_c, end);
    check_range("vector->string", c_fix(start), 0, vector_size(v));
    check_range("vector->string", c_fix(end), c_fix(start), vector_size(v));
    int size = c_fix(end) - c_fix(start) + 2;
    char* s = scm_new_atomic(size, char);
    for(int i = c_fix(start), j = 0; i <= c_fix(end); i++, j++) {
        scmval x = vector_ref(v, i);
        if(!is_char(x)) error(type_error_type, "vector->string: vector element %s is not a character", scm_to_cstr(x));
        s[j] = c_char(x);
    }
    s[size-1] = '\0';
    return s_str(s);
}

static scmval scm_string_to_vector(scmval s, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(string_length(s) - 1));
    check_arg("string->vector", string_c, s);
    check_arg("string->vector", fixnum_c, start);
    check_arg("string->vector", fixnum_c, end);
    check_range("string->vector", c_fix(start), 0, string_length(s));
    check_range("string->vector", c_fix(end), c_fix(start), string_length(s));
    int size = c_fix(end) - c_fix(start) + 1;
    scm_vector_t* v = scm_new(scm_vector_t);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    char* str = c_str(s);
    for(int i = c_fix(start), j = 0; i <= c_fix(end); i++, j++) {
        v->elts[j] = s_char(str[i]);
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

static scmval scm_vector_copy(scmval v, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(vector_size(v) - 1));
    check_arg("vector-copy", vector_c, v);
    check_arg("vector-copy", fixnum_c, start);
    check_arg("vector-copy", fixnum_c, end);
    check_range("vector-copy", c_fix(start), 0, vector_size(v));
    check_range("vector-copy", c_fix(end), c_fix(start), vector_size(v));
    scm_vector_t* copy = scm_new(scm_vector_t);
    copy->size = c_fix(end) - c_fix(start) + 1;
    copy->elts = scm_new_array(copy->size, scmval);
    memcpy(copy->elts, get_vector(v)->elts+c_fix(start), copy->size*sizeof(scmval));
    return make_ptr(SCM_TYPE_VECTOR, copy);
}

static scmval scm_vector_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end, s_fix(vector_size(from) - 1));
    check_arg("vector-copy!", vector_c, to);
    check_arg("vector-copy!", fixnum_c, at);
    check_arg("vector-copy!", vector_c, from);
    check_arg("vector-copy!", fixnum_c, start);
    check_arg("vector-copy!", fixnum_c, end);
    check_range("vector-copy!", c_fix(at), 0, vector_size(to));
    check_range("vector-copy!", c_fix(start), 0, vector_size(from));
    check_range("vector-copy!", c_fix(end), c_fix(start), vector_size(from));
    int size = c_fix(end) - c_fix(start) + 1;
    if((vector_size(to) - c_fix(at)) < size)
        error(range_error_type, "vector-copy!: cannot fit %d elements in %s starting at index %d", 
                size, scm_to_cstr(to), c_fix(at));
    memcpy(get_vector(to)->elts + c_fix(at),
           get_vector(from)->elts + c_fix(start),
           size*sizeof(scmval));
    return scm_void;
}

static scmval scm_vector_append(int argc, scmval* argv) {
    check_args("vector-append", vector_c, argc, argv);
    int size = 0;
    for(int i = 0; i < argc; i++) {
        size += vector_size(argv[i]);
    }
    scm_vector_t* v = scm_new(scm_vector_t);
    v->size = size;
    v->elts = scm_new_array(size, scmval);
    int offset = 0;
    for(int i = 0; i < argc; i++) {
        memcpy(v->elts + offset,
               get_vector(argv[i])->elts,
               vector_size(argv[i])*sizeof(scmval));
        offset += vector_size(argv[i]);
    }
    return make_ptr(SCM_TYPE_VECTOR, v);
}

static scmval scm_vector_fill(scmval v, scmval fill, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end,   s_fix(vector_size(v) - 1));
    check_arg("vector-fill!", vector_c, v);
    check_arg("vector-fill!", fixnum_c, start);
    check_arg("vector-fill!", fixnum_c, end);
    check_range("vector-fill!", c_fix(start), 0, vector_size(v));
    check_range("vector-fill!", c_fix(end), c_fix(start), vector_size(v));
    for(int i = c_fix(start); i <= c_fix(end); i++) {
        vector_set(v, i, fill);
    }
    return scm_void;
}

// initialization
void init_vector(scmval env) {
    define(env, "vector?",           scm_vector_p,           arity_exactly(1));
    define(env, "make-vector",       scm_make_vector,        arity_or(1, 2));
    define(env, "vector",            scm_vector,             arity_at_least(0));
    define(env, "vector-length",     scm_vector_length,      arity_exactly(1));
    define(env, "vector-ref",        scm_vector_ref,         arity_exactly(2));
    define(env, "vector-set!",       scm_vector_set,         arity_exactly(3));
    define(env, "vector->list",      scm_vector_to_list,     arity_between(1, 3));
    define(env, "list->vector",      scm_list_to_vector,     arity_exactly(1));
    define(env, "vector->string",    scm_vector_to_string,   arity_between(1, 3));
    define(env, "string->vector",    scm_string_to_vector,   arity_between(1, 3));
    define(env, "vector-copy",       scm_vector_copy,        arity_between(1, 3));
    define(env, "vector-copy!",      scm_vector_mcopy,       arity_between(1, 3));
    define(env, "vector-append",     scm_vector_append,      arity_at_least(2));
    define(env, "vector-fill!",      scm_vector_fill,        arity_between(2, 4));
}

