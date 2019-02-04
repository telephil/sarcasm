#include "scm.h"

// constructor
scmval make_primitive(const char* name, primitive_f fn, arity_t arity) {
    scm_primitive_t* primitive = scm_gc_malloc(sizeof(scm_primitive_t));
    primitive->name  = s_str(name);
    primitive->f     = fn;
    primitive->arity = arity;
    return make_ptr(SCM_TYPE_PRIMITIVE, primitive);
}

scmval make_closure(scmval name, int argc, scmval* argv, scmval env, scmval body) {
    scm_closure_t* closure = scm_gc_malloc(sizeof(scm_closure_t));
    closure->name = name;
    closure->argc = argc;
    closure->argv = argv;
    closure->env  = env;
    closure->body = body;
    return make_ptr(SCM_TYPE_CLOSURE, closure);
}

scmval make_continuation() {
    scm_continuation_t* cont = scm_gc_malloc(sizeof(scm_continuation_t));
    cont->value = scm_undef;
    return make_ptr(SCM_TYPE_CONTINUATION, cont);
}

// arity
static void arity_error(scmval v, int argc) {
    arity_t arity = primitive_arity(v);
    char* n = c_str(primitive_name(v));
    char* m;
    switch(arity.type) {
        case ARITY_EXACTLY:
            asprintf(&m, "exactly %d argument%s", arity.min, arity.min <= 1 ? "" : "s");
            break;
        case ARITY_OR:
            asprintf(&m, "%d or %d arguments", arity.min, arity.max);
            break;
        case ARITY_BETWEEN:
            asprintf(&m, "%d to %d arguments", arity.min, arity.max);
            break;
        case ARITY_AT_LEAST:
            asprintf(&m, "at least %d argument%s", arity.min, arity.min <= 1 ? "" : "s");
            break;
    }
    error(arity_error_type, "%s expects %s but received %d", n, m, argc);
}

void check_arity(scmval v, int argc) {
    bool match = false;
    arity_t arity = primitive_arity(v);
    switch(arity.type) {
        case ARITY_EXACTLY:  match = (argc == arity.min); break;
        case ARITY_OR:       match = (argc == arity.min || argc == arity.max); break;
        case ARITY_BETWEEN:  match = (arity.min <= argc && argc <= arity.max); break;
        case ARITY_AT_LEAST: match = (argc >= arity.min); break;
    }
    if(!match)
        arity_error(v, argc);
}

int argc_from_arity(scmval primitive, int len) {
    int argc;
    arity_t arity = primitive_arity(primitive);
    switch(arity.type) {
        case ARITY_EXACTLY: // already checked
        case ARITY_AT_LEAST:
            argc = len;
            break;
        case ARITY_OR:
        case ARITY_BETWEEN:
            argc = arity.max; // need to pad up to arity.max
            break;
    }
    return argc;
}

