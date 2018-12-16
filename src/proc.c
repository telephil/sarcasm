#include "scm.h"

// constructor
scmval make_prim(const char* name, scm_prim_fun fun, arity_t arity, int n, ...) {
    scmval p;
    va_list ap;
    va_start(ap, n);
    p = make_primv(name, fun, arity, n, ap);
    va_end(ap);
    return p;
}

scmval make_primv(const char* name, scm_prim_fun fun, arity_t arity, int n, va_list ap) {
    scm_prim_t* prim = scm_new(scm_prim_t);
    prim->name  = make_string(name);
    prim->fun   = fun;
    prim->arity = arity;
    prim->contracts = scm_new_array(n, contract_t);
    prim->contract_count = n;
    for(int i = 0; i < n; i++) {
        prim->contracts[i] = va_arg(ap, contract_t);
    }
    return make_ptr(SCM_TYPE_PRIM, prim);
}

// arity
static scmval arity_error(scmval v, int argc) {
    arity_t arity = prim_arity(v);
    char* n = string_to_cstr(prim_name(v));
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
    return error(arity_error_type, "%s expects %s but received %d", n, m, argc);
}

static void ensure_arity(scm_ctx_t* ctx, scmval v, int argc) {
    scmval e;
    bool match = false;
    arity_t arity = prim_arity(v);
    switch(arity.type) {
        case ARITY_EXACTLY:
            match = (argc == arity.min);
            break;
        case ARITY_OR:
            match = (argc == arity.min || argc == arity.max);
            break;
        case ARITY_BETWEEN:
            match = (arity.min <= argc && argc <= arity.max);
            break;
        case ARITY_AT_LEAST:
            match = (argc >= arity.min);
            break;
    }
    if(!match) {
        e = arity_error(v, argc);
        throw(ctx, e);
    }
}

// Contract
static void ensure_contract(scm_ctx_t* ctx, scmval v, int argc, const scmval* argv) {
    scmval e;
    contract_t* contracts = prim_contracts(v);
    int ncontracts = prim_contract_count(v);
    for(int i = 0, n = 0; i < argc; i++) {
        if(!contracts[n].pred(argv[i])) {
            e = error(contract_error_type,
                      "%s: contract violation (received XXX but expected %s)",
                      string_to_cstr(prim_name(v)),
                      contracts[n].name);
            throw(ctx, e);
        }
        if((n < ncontracts - 1) || ncontracts >= argc) {
            ++n;
        }
    }
}

scmval apply(scm_ctx_t* ctx, scmval prim, scmval args) {
    scmval r, v;
    size_t l, i;
    l = list_length(args);
    push_frame(ctx, l);
    ensure_arity(ctx, prim, l);
    for(i = 0, v = args; !is_null(v); i++, v = cdr(v)) {
        arg_set(ctx, i, car(v));
    }
    ensure_contract(ctx, prim, l, ctx->stack->argv);
    r = prim_fun(prim)(ctx);
    pop_frame(ctx);
    return r;
}

