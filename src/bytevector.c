#include "scm.h"

scmval make_bytevector(size_t size, scmval initial) {
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = size;
    b->elts = scm_new_array(size, scmbyte);
    for(int i = 0; i < size; i++) {
        b->elts[i] = fixnum_value(initial);
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

scmval make_bytevector_from_list(scmfix size, scmval l) {
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    if(size < 0)
        size = list_length(l);
    b->size = size;
    b->elts = scm_new_array(size, scmbyte);
    int i;
    scmval c;
    for(c = l, i = 0; !is_null(c); c = cdr(c), i++) {
        b->elts[i] = fixnum_value(car(c));
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

// standard library
static scmval scm_bytevector_p(scmval v) {
    return scm_bool(is_bytevector(v));
}

static scmval scm_make_bytevector(scmval size, scmval initial) {
    check_arg("make-bytevector", fixnum_c, size);
    opt_arg(initial, make_fixnum(0));
    return make_bytevector(fixnum_value(size), initial);
}

static scmval scm_bytevector(scmfix argc, scmval* argv) {
    check_args("bytevector", byte_c, argc, argv);
    scmval b = make_bytevector(argc, make_fixnum(0));
    for(int i = 0; i < argc; i++) {
        bytevector_set(b, i, argv[i]);
    }
    return b;
}

static scmval scm_bytevector_length(scmval b) {
    check_arg("bytevector-length", bytevector_c, b);
    return make_fixnum(bytevector_size(b));
}

static scmval scm_bytevector_ref(scmval b, scmval k) {
    check_arg("bytevector-u8-ref", bytevector_c, b);
    check_arg("bytevector-u8-ref", fixnum_c, k);
    check_range("bytevector-u8-ref", fixnum_value(k), 0, bytevector_size(b));
    return bytevector_ref(b, fixnum_value(k));
}

static scmval scm_bytevector_set(scmval b, scmval k, scmval byte) {
    check_arg("bytevector-u8-set!", bytevector_c, b);
    check_arg("bytevector-u8-set!", fixnum_c, k);
    check_arg("bytevector-u8-set!", byte_c, byte);
    check_range("bytevector-u8-set!", fixnum_value(k), 0, bytevector_size(b));
    bytevector_set(b, fixnum_value(k), byte);
    return scm_undef;
}

static scmval scm_bytevector_copy(scmval b, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end, make_fixnum(bytevector_size(b) - 1));
    check_arg("bytevector-copy", bytevector_c, b);
    check_arg("bytevector-copy", fixnum_c, start);
    check_arg("bytevector-copy", fixnum_c, end);
    check_range("bytevector-copy", fixnum_value(start), 0, bytevector_size(b));
    check_range("bytevector-copy", fixnum_value(end), fixnum_value(start), bytevector_size(b));
    scm_bytevector_t* copy = scm_new(scm_bytevector_t);
    copy->size = fixnum_value(end) - fixnum_value(start) + 1;
    copy->elts = scm_new_array(copy->size, scmbyte);
    memcpy(copy->elts, get_bytevector(b)->elts+fixnum_value(start), copy->size);
    return make_ptr(SCM_TYPE_BYTEVECTOR, copy);
}

static scmval scm_bytevector_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end, make_fixnum(bytevector_size(from) - 1));
    check_arg("bytevector-copy!", bytevector_c, to);
    check_arg("bytevector-copy!", fixnum_c, at);
    check_arg("bytevector-copy!", bytevector_c, from);
    check_arg("bytevector-copy!", fixnum_c, start);
    check_arg("bytevector-copy!", fixnum_c, end);
    check_range("bytevector-copy!", fixnum_value(at), 0, bytevector_size(to));
    check_range("bytevector-copy!", fixnum_value(start), 0, bytevector_size(from));
    check_range("bytevector-copy!", fixnum_value(end), fixnum_value(start), bytevector_size(from));
    if((bytevector_size(to) - fixnum_value(at)) < (fixnum_value(end) - fixnum_value(start)))
        error(range_error_type, "bytevector-copy!: cannot fit %d elements in %s starting at index %d", 
                (fixnum_value(end)-fixnum_value(start)), string_value(scm_to_string(to)), fixnum_value(at));
    memcpy(get_bytevector(to)->elts + fixnum_value(at),
           get_bytevector(from)->elts + fixnum_value(start),
           fixnum_value(end) - fixnum_value(start) + 1);
    return scm_undef;
}

static scmval scm_bytevector_append(scmfix argc, scmval* argv) {
    check_args("bytevector-append", bytevector_c, argc, argv);
    scmfix size = 0;
    for(int i = 0; i < argc; i++) {
        size += bytevector_size(argv[i]);
    }
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = size;
    b->elts = scm_new_array(size, scmbyte);
    scmfix offset = 0;
    for(int i = 0; i < argc; i++) {
        memcpy(b->elts + offset,
               get_bytevector(argv[i])->elts,
               bytevector_size(argv[i]));
        offset += bytevector_size(argv[i]);
    }
    return make_ptr(SCM_TYPE_BYTEVECTOR, b);
}

static scmval scm_utf8_to_string(scmval b, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end  , make_fixnum(bytevector_size(b)));
    check_arg("utf8->string", bytevector_c, b);
    check_arg("utf8->string", fixnum_c, start);
    check_arg("utf8->string", fixnum_c, end);
    check_range("utf8->string", fixnum_value(start), 0, bytevector_size(b));
    check_range("utf8->string", fixnum_value(end), fixnum_value(start), bytevector_size(b));
    scmfix size = (fixnum_value(end)-fixnum_value(start)) + 1;
    scm_char_t* s = scm_new_array(size, scm_char_t);
    memcpy(s, get_bytevector(b)->elts + fixnum_value(start), size);
    s[size] = '\0';
    return make_string(s);
}

static scmval scm_string_to_utf8(scmval s, scmval start, scmval end) {
    opt_arg(start, make_fixnum(0));
    opt_arg(end  , make_fixnum(string_length(s)));
    check_arg("string->utf8", string_c, s);
    check_arg("string->utf8", fixnum_c, start);
    check_arg("string->utf8", fixnum_c, end);
    check_range("string->utf8", fixnum_value(start), 0, string_length(s));
    check_range("string->utf8", fixnum_value(end), fixnum_value(start), string_length(s));
    scm_bytevector_t* b = scm_new(scm_bytevector_t);
    b->size = fixnum_value(end) - fixnum_value(start);
    b->elts = scm_new_array(b->size, scmbyte);
    memcpy(b->elts, string_to_cstr(s)+fixnum_value(start), b->size);
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


