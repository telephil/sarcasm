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
scmval intern(scm_ctx_t* ctx, const char* name) {
    scmval r, s;
    s = make_symbol(name);
    r = dict_ref(ctx->symbols, s);
    if(is_undef(r)) {
        dict_set(ctx->symbols, s, s);
        r = s;
    }
    return r;
}

// standard library
static scmval scm_symbol_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_symbol(v))
        return scm_true;
    return scm_false;
}

static scmval scm_symbol_equal_p(scm_ctx_t* ctx) {
    int argc;
    scmval *argv;
    arg_ref_list(ctx, &argc, &argv);
    scmval s = argv[0];
    for(int i = 1; i < argc; i++) {
        if(!is_eq(s, argv[i]))
            return scm_false;
    }
    return scm_true;
}

static scmval scm_symbol_to_string(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    v.type = SCM_TYPE_STRING; // cheapest conversion ever
    return v;
}

static scmval scm_string_to_symbol(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    v.type = SCM_TYPE_SYMBOL;
    return v;
}

// initialization
void init_symbol(scm_ctx_t* ctx) {
    define(ctx, "symbol?", scm_symbol_p, arity_exactly(1), 1, any_c);
    define(ctx, "symbol=?", scm_symbol_equal_p, arity_at_least(2), 1, symbol_c);
    define(ctx, "symbol->string", scm_symbol_to_string, arity_exactly(1), 1, symbol_c);
    define(ctx, "string->symbol", scm_string_to_symbol, arity_exactly(1), 1, string_c);
    
    scm_undef = make_val(SCM_TYPE_UNDEF);
}

