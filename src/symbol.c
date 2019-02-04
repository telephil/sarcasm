#include "scm.h"

// constructor
scmval make_symbol(const char* s) {
    scm_string_t* c = scm_gc_malloc(sizeof(scm_string_t));
    c->value = scm_gc_strdup(s);
    return make_ptr(SCM_TYPE_SYMBOL, c);
}

// standard library
static scmval scm_symbol_p(scmval v) {
    return s_bool(is_symbol(v));
}

static scmval scm_symbol_equal_p(int argc, scmval* argv) {
    check_args("symbol=?", symbol_c, argc, argv);
    scmval s = argv[0];
    for(int i = 1; i < argc; i++) {
        if(!is_eq(s, argv[i]))
            return scm_false;
    }
    return scm_true;
}

static scmval scm_symbol_to_string(scmval v) {
    check_arg("symbol->string", symbol_c, v);
    v.type = SCM_TYPE_STRING; // cheapest conversion ever
    return v;
}

static scmval scm_string_to_symbol(scmval v) {
    check_arg("string->symbol", string_c, v);
    v.type = SCM_TYPE_SYMBOL;
    return v;
}

// initialization
void init_symbol(scmval env) {
    define(env, "symbol?",          scm_symbol_p,           arity_exactly(1));
    define(env, "symbol=?",         scm_symbol_equal_p,     arity_at_least(2));
    define(env, "symbol->string",   scm_symbol_to_string,   arity_exactly(1));
    define(env, "string->symbol",   scm_string_to_symbol,   arity_exactly(1));
}

////////////////////////////////////////////////////////////////////////////////
// INTERN POOL
////////////////////////////////////////////////////////////////////////////////
static scm_dict_t* scm_g_symbols;

#define define_symbol(CNAME,SNAME)  scmval CNAME;
#include "scm/symbols.inc"
#undef define_symbol

void init_intern_pool() {
    scm_g_symbols = make_dict();
#define define_symbol(CNAME,SNAME)  CNAME = intern(SNAME);
#include "scm/symbols.inc"
#undef define_symbol
}

scmval intern(const char* name) {
    scmval s = make_symbol(name);
    scmval r = dict_maybe_set(scm_g_symbols, s, s);
    return r;
}

