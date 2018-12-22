#include "scm.h"

scmval make_bytevector(size_t size, scmval initial) {
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = size;
    b->elts = scm_new_array(size, byte);
    for(int i = 0; i < size; i++) {
        b->elts[i] = c_fix(initial);
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

scmval make_bytevector_from_list(int size, scmval l) {
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    if(size < 0)
        size = list_length(l);
    b->size = size;
    b->elts = scm_new_array(size, byte);
    int i;
    scmval c;
    for(c = l, i = 0; !is_null(c); c = cdr(c), i++) {
        b->elts[i] = c_fix(car(c));
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

// standard library
static scmval scm_bytevector_p(scmval v) {
    return scm_bool(is_bytevector(v));
}

static scmval scm_make_bytevector(scmval size, scmval initial) {
    check_arg("make-bytevector", fixnum_c, size);
    opt_arg(initial, scm_0);
    return make_bytevector(c_fix(size), initial);
}

static scmval scm_bytevector(int argc, scmval* argv) {
    check_args("bytevector", byte_c, argc, argv);
    scmval b = make_bytevector(argc, scm_0);
    for(int i = 0; i < argc; i++) {
        bytevector_set(b, i, argv[i]);
    }
    return b;
}

static scmval scm_bytevector_length(scmval b) {
    check_arg("bytevector-length", bytevector_c, b);
    return scm_fix(bytevector_size(b));
}

static scmval scm_bytevector_ref(scmval b, scmval k) {
    check_arg("bytevector-u8-ref", bytevector_c, b);
    check_arg("bytevector-u8-ref", fixnum_c, k);
    check_range("bytevector-u8-ref", c_fix(k), 0, bytevector_size(b));
    return bytevector_ref(b, c_fix(k));
}

static scmval scm_bytevector_set(scmval b, scmval k, scmval byte) {
    check_arg("bytevector-u8-set!", bytevector_c, b);
    check_arg("bytevector-u8-set!", fixnum_c, k);
    check_arg("bytevector-u8-set!", byte_c, byte);
    check_range("bytevector-u8-set!", c_fix(k), 0, bytevector_size(b));
    bytevector_set(b, c_fix(k), byte);
    return scm_undef;
}

static scmval scm_bytevector_copy(scmval b, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end, scm_fix(bytevector_size(b) - 1));
    check_arg("bytevector-copy", bytevector_c, b);
    check_arg("bytevector-copy", fixnum_c, start);
    check_arg("bytevector-copy", fixnum_c, end);
    check_range("bytevector-copy", c_fix(start), 0, bytevector_size(b));
    check_range("bytevector-copy", c_fix(end), c_fix(start), bytevector_size(b));
    scm_bytevector_t* copy = scm_new(scm_bytevector_t);
    copy->size = c_fix(end) - c_fix(start) + 1;
    copy->elts = scm_new_array(copy->size, byte);
    memcpy(copy->elts, get_bytevector(b)->elts+c_fix(start), copy->size);
    return make_ptr(SCM_TYPE_BYTEVECTOR, copy);
}

static scmval scm_bytevector_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end, scm_fix(bytevector_size(from) - 1));
    check_arg("bytevector-copy!", bytevector_c, to);
    check_arg("bytevector-copy!", fixnum_c, at);
    check_arg("bytevector-copy!", bytevector_c, from);
    check_arg("bytevector-copy!", fixnum_c, start);
    check_arg("bytevector-copy!", fixnum_c, end);
    check_range("bytevector-copy!", c_fix(at), 0, bytevector_size(to));
    check_range("bytevector-copy!", c_fix(start), 0, bytevector_size(from));
    check_range("bytevector-copy!", c_fix(end), c_fix(start), bytevector_size(from));
    if((bytevector_size(to) - c_fix(at)) < (c_fix(end) - c_fix(start)))
        error(range_error_type, "bytevector-copy!: cannot fit %d elements in %s starting at index %d", 
                (c_fix(end)-c_fix(start)), scm_to_cstr(to), c_fix(at));
    memcpy(get_bytevector(to)->elts + c_fix(at),
           get_bytevector(from)->elts + c_fix(start),
           c_fix(end) - c_fix(start) + 1);
    return scm_undef;
}

static scmval scm_bytevector_append(int argc, scmval* argv) {
    check_args("bytevector-append", bytevector_c, argc, argv);
    int size = 0;
    for(int i = 0; i < argc; i++) {
        size += bytevector_size(argv[i]);
    }
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = size;
    b->elts = scm_new_array(size, byte);
    int offset = 0;
    for(int i = 0; i < argc; i++) {
        memcpy(b->elts + offset,
               get_bytevector(argv[i])->elts,
               bytevector_size(argv[i]));
        offset += bytevector_size(argv[i]);
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

static scmval scm_utf8_to_string(scmval b, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end  , scm_fix(bytevector_size(b)));
    check_arg("utf8->string", bytevector_c, b);
    check_arg("utf8->string", fixnum_c, start);
    check_arg("utf8->string", fixnum_c, end);
    check_range("utf8->string", c_fix(start), 0, bytevector_size(b));
    check_range("utf8->string", c_fix(end), c_fix(start), bytevector_size(b));
    int size = (c_fix(end)-c_fix(start)) + 1;
    char* s = scm_new_array(size, char);
    memcpy(s, get_bytevector(b)->elts + c_fix(start), size);
    s[size] = '\0';
    return scm_str(s);
}

static scmval scm_string_to_utf8(scmval s, scmval start, scmval end) {
    opt_arg(start, scm_0);
    opt_arg(end  , scm_fix(string_length(s)));
    check_arg("string->utf8", string_c, s);
    check_arg("string->utf8", fixnum_c, start);
    check_arg("string->utf8", fixnum_c, end);
    check_range("string->utf8", c_fix(start), 0, string_length(s));
    check_range("string->utf8", c_fix(end), c_fix(start), string_length(s));
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = c_fix(end) - c_fix(start);
    b->elts = scm_new_array(b->size, byte);
    memcpy(b->elts, c_cstr(s)+c_fix(start), b->size);
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

void init_bytevector() {
    define("bytevector?",        scm_bytevector_p,       arity_exactly(1));
    define("make-bytevector",    scm_make_bytevector,    arity_or(1, 2));
    define("bytevector",         scm_bytevector,         arity_at_least(0));
    define("bytevector-length",  scm_bytevector_length,  arity_exactly(1));
    define("bytevector-u8-ref",  scm_bytevector_ref,     arity_exactly(2));
    define("bytevector-u8-set!", scm_bytevector_set,     arity_exactly(3));
    define("bytevector-copy",    scm_bytevector_copy,    arity_between(1, 3));
    define("bytevector-copy!",   scm_bytevector_mcopy,   arity_between(3, 5));
    define("bytevector-append",  scm_bytevector_append,  arity_at_least(2));
    define("utf8->string",       scm_utf8_to_string,     arity_between(1, 3));
    define("string->utf8",       scm_string_to_utf8,     arity_between(1, 3));
}


