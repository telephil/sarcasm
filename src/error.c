#include "scm.h"

// globals
scmval   scm_g_lasterr;
jmp_buf  scm_g_errbuf;

scmval exn_error_type;
scmval cexn_error_type;
scmval range_error_type;
scmval arity_error_type;
scmval type_error_type;
scmval file_error_type;

// constructor
scmval make_error(scmval type, scmval message, scmval irritants) {
    scm_error_t* e = scm_new(scm_error_t);
    e->type      = type;
    e->message   = message;
    e->irritants = irritants;
    return make_ptr(SCM_TYPE_ERROR, e);
}

// standard library
void raise(scmval e) {
    scm_g_lasterr = e;
    longjmp(scm_g_errbuf, 1);
}

// standard library
static scmval scm_raise(scmval obj) {
    scmval err = make_error(exn_error_type, scm_undef, list1(obj));
    raise(err);
    return scm_undef;
}

static scmval scm_raise_continuable(scmval obj) {
    scmval err = make_error(cexn_error_type, scm_undef, list1(obj));
    raise(err);
    return scm_undef;
}

static scmval scm_error(int argc, scmval* argv) {
    check_arg("error", string_c, argv[0]);
    scmval irrs = scm_null;
    if(argc > 1) {
        for(int i = argc - 1; i > 0; i--) {
            irrs = cons(argv[i], irrs);
        }
    }
    scmval err = make_error(scm_undef, argv[0], irrs);
    raise(err);
    return scm_undef; // not reached
}

static scmval scm_error_object_p(scmval obj) {
    return scm_bool(is_error(obj));
}

static scmval scm_error_object_message(scmval obj) {
    check_arg("error-object-message", error_c, obj);
    return error_message(obj);
}

static scmval scm_error_object_irritants(scmval obj) {
    check_arg("error-object-irritants", error_c, obj);
    return error_irritants(obj);
}

static scmval scm_file_error_p(scmval v) {
    return scm_bool(is_error(v) && is_eq(error_type(v), file_error_type));
}

// initialization
void init_errors(scmval env) {
    define(env, "raise",                    scm_raise,                  arity_exactly(1));
    define(env, "raise-continuable",        scm_raise_continuable,      arity_exactly(1));
    define(env, "error",                    scm_error,                  arity_at_least(1));
    define(env, "file-error?",              scm_file_error_p,           arity_exactly(1));
    define(env, "error-object-p",           scm_error_object_p,         arity_exactly(1));
    define(env, "error-object-message",     scm_error_object_message,   arity_exactly(1));
    define(env, "error-object-irritants",   scm_error_object_irritants, arity_exactly(1));

    exn_error_type   = intern("exception");
    cexn_error_type  = intern("continuable exception");
    type_error_type  = intern("type-error");
    range_error_type = intern("range-error");
    arity_error_type = intern("arity-error");
    file_error_type  = intern("file-error");
}

