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
scmval intern(const char* name) {
    scmval r, s;
    s = make_symbol(name);
    r = dict_ref(scm_context.symbols, s);
    if(is_undef(r)) {
        dict_set(scm_context.symbols, s, s);
        r = s;
    }
    return r;
}

// standard library
static scmval scm_symbol_p(scmval v) {
    return scm_bool(is_symbol(v));
}

static scmval scm_symbol_equal_p(scmfix argc, scmval* argv) {
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
void init_symbol() {
    define("symbol?", scm_symbol_p, arity_exactly(1));
    define("symbol=?", scm_symbol_equal_p, arity_at_least(2));
    define("symbol->string", scm_symbol_to_string, arity_exactly(1));
    define("string->symbol", scm_string_to_symbol, arity_exactly(1));
    
    scm_undef = make_val(SCM_TYPE_UNDEF);
}

