#include "scm.h"

// globals
scmval range_error_type;
scmval arity_error_type;
scmval contract_error_type;

// constructor
scmval make_error(scmval type, scmval message) {
    scm_error_t* e = scm_new(scm_error_t);
    e->type = type;
    e->message = message;
    return make_ptr(SCM_TYPE_ERROR, e);
}

// standard library
void throw(scm_ctx_t* ctx, scmval e) {
    set_error(ctx, e);
    longjmp(ctx->err_buf, 1);
}

// initialization
void init_errors(scm_ctx_t* ctx) {
    range_error_type = intern(ctx, make_symbol("range-error"));
    arity_error_type = intern(ctx, make_symbol("arity-error"));
    contract_error_type = intern(ctx, make_symbol("contract-error"));
}

