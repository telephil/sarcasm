#include "scm.h"

// globals
scmval scm_undef;

// constructor
scmval make_symbol(const char* s) {
    scm_string_t* c = scm_new(scm_string_t);
    c->value = CORD_from_char_star(s);
    return make_ptr(SCM_TYPE_SYMBOL, c);
}

// standard library
scmval intern(scm_ctx_t* ctx, scmval s) {
    scmval r;
    r = dict_ref(ctx->symbols, s);
    if(is_undef(r)) {
        dict_set(ctx->symbols, s, s);
        r = s;
    }
    return r;
}

// initialization
void init_symbol(scm_ctx_t* ctx) {
    scm_undef = make_val(SCM_TYPE_UNDEF);
}

