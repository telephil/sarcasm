#include "scm.h"

static scmval scm_g_report_environment;
static scmval scm_g_interaction_environment;
static scmval scm_g_null_environment;

// constructor
scmval make_env(scmval n) {
    scm_env_t* e = scm_new(scm_env_t);
    e->globals   = make_dict();
    e->bindings  = make_dict();
    e->next = n;
    return make_ptr(SCM_TYPE_ENV, e);
}

// utilities
void define(scmval env, const char* name, primitive_f fun, arity_t arity) {
    scmval s = make_primitive(name, fun, arity);
    dict_set(env_globals(env), intern(name), s);
}

scmval lookup(scmval e, scmval s) {
    scmval v;
    while(!is_undef(e)) {
        v = dict_ref(env_bindings(e), s);
        if(!is_undef(v)) {
            return v;
        }
        if(is_undef(env_next(e)))
            break;
        e = env_next(e);
    }
    v = dict_ref(env_globals(e), s);
    return v;
}

void set(scmval e, scmval s, scmval v) {
    dict_set(env_globals(e), s, v);
}

bool update(scmval e, scmval s, scmval v) {
    scmval v0;
    while(!is_undef(e)) {
        v0 = dict_ref(env_bindings(e), s);
        if(!is_undef(v0)) {
            dict_set(env_bindings(e), s, v);
            return true;
        }
        if(is_undef(env_next(e)))
            break;
        e = env_next(e);
    }
    v0 = dict_ref(env_globals(e), s);
    if(!is_undef(v0)) {
        dict_set(env_globals(e), s, v);
        return true;
    }
    return false;
}


void bind(scmval e, scmval s, scmval v) {
    dict_set(env_bindings(e), s, v);
}

void import(scmval name, scmval env) {
    scmval lib = load_library(name, env);
    if(is_undef(lib))
        error(intern("error"), "library '%s' not found", scm_to_cstr(name));
    // FIXME
    // atm we copy everything otherwise the non-exported symbols
    // are not visible.
    // THIS IS A MAJOR ISSUE
    dict_copy(env_globals(env), library_symbols(lib));
    /*
    foreach(export, library_exports(lib)) {
        scmval value = dict_ref(library_symbols(lib), export);
        set(env, export, value);
    }
    */
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
scmval scm_environment(int argc, scmval* argv) {
    scmval env = make_env(scm_undef);
    for(int i = 0; i < argc; i++) {
        import(argv[i], env);
    }
    return env;
}

scmval scm_report_environment(scmval version) {
    check_arg("scheme-report-environment", fixnum_c, version);
    if(c_fix(version) != 5)
        error(intern("error"), "unsupported version %d", c_fix(version));
    return scm_g_report_environment;
}

scmval scm_null_environment(scmval version) {
    check_arg("null-environment", fixnum_c, version);
    if(c_fix(version) != 5)
        error(intern("error"), "unsupported version %d", c_fix(version));
    return scm_g_null_environment;
}

scmval scm_interaction_environment() {
    return scm_g_interaction_environment;
}

void init_env(scmval env) {
    define(env, "environment",               scm_environment,               arity_at_least(0));
    define(env, "scheme-report-environment", scm_report_environment,        arity_exactly(1));
    define(env, "null-environment",          scm_null_environment,          arity_exactly(1));
    define(env, "interaction-environment",   scm_interaction_environment,   arity_exactly(0));
}

void post_init_env() {
    scmval args[] = { read_from_string("(sarcasm core)"),
                      read_from_string("(scheme cxr)") };
    scm_g_report_environment        = scm_environment(1, args); // FIXME
    scm_g_interaction_environment   = scm_environment(2, args);
    scm_g_null_environment          = make_env(scm_undef);
}
