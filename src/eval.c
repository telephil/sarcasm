#include "scm.h"

static scmval scm_define;
static scmval scm_lambda;
static scmval scm_if;
static scmval scm_set;

void init_eval(scm_ctx_t* ctx) {
    scm_define = intern(ctx, "define");
    scm_if = intern(ctx, "if");
    scm_set = intern(ctx, "set!");
    scm_lambda = intern(ctx, "lambda");
}

// (map (eval v e))
scmval map_eval(scm_ctx_t* ctx, scmval v, scmval e) {
    if(is_null(v))
        return scm_null;
    scmval r = eval(ctx, car(v), e);
    return cons(r, map_eval(ctx, cdr(v), e));
}

#define dbg(P,V) { printf(">>> " P ": '"); write(ctx->current_output_port, V, WRITE_MODE_WRITE); printf("'\n"); }

scmval eval(scm_ctx_t* ctx, scmval v, scmval e) {
    scmval r = v;

    if(is_symbol(v)) {
        r = lookup(ctx, e, v);
        if(is_undef(r))
            throw(ctx, error(intern(ctx, "error"), "undefined symbol '%s'", string_value(v)));
    } else if(is_pair(v) && !is_null(v)) {
        scmval s = car(v);
        if(is_eq(s, scm_define)) {
            scmval k = cadr(v);
            if(is_pair(k)) { // (define (name <args>)...
                scmval n = car(k);
                scmval a = cdr(k);
                scmval b = caddr(v);
                scmval *argv = NULL;
                scm_fixnum_t argc = list_length(a);
                if(argc > 0) {
                    argv = scm_new_array(argc, scmval);
                    int i = 0;
                    for(scmval l = a; !is_null(l); l = cdr(l)) {
                        argv[i++] = car(l);
                    }
                }
                r = make_closure(argc, argv, make_env(e), b);
                dict_set(ctx->globals, n, r);
            } else { // (define name...
                scmval a = eval(ctx, caddr(v), e);
                dict_set(ctx->globals, k, a);
                r = a;
            }
        } else if(is_eq(s, scm_if)) {
            scmval test = cadr(v);
            scmval t = caddr(v);
            scmval f = cadddr(v);
            if(!is_false(eval(ctx, test, e)))
                r = eval(ctx, t, e);
            else
                r = eval(ctx, f, e);
        } else if(is_eq(s, scm_quote)) {
            r = cadr(v);
        } else {
            scmval f = is_callable(s) ? s : eval(ctx, s, e);
            if(is_prim(f)) {
                scmval a = map_eval(ctx, cdr(v), e);
                r = apply(ctx, f, a);
            } else if(is_closure(f)) {
                scm_fixnum_t argc = closure_argc(f);
                scm_fixnum_t largc = list_length(cdr(v));
                if(argc != largc) {
                    throw(ctx, error(intern(ctx, "eval"),
                                "invalid argument count (expected: %d but received: %d)", argc, largc));
                }
                scmval *argv = closure_argv(f);
                scmval largv = cdr(v);
                scmval env = make_env(closure_env(f));
                for(int i = 0; i < argc; i++, largv = cdr(largv)) {
                    scmval a = eval(ctx, car(largv), e);
                    bind(ctx, env, argv[i], a);
                }
                r = eval(ctx, closure_body(f), env);
            }
        }
    }

    return r;
}

