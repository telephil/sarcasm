#include "scm.h"

static scmval scm_apply;
static scmval scm_define;
static scmval scm_lambda;
static scmval scm_if;
static scmval scm_set;
static scmval scm_begin;

static void   list_to_args(scmval, scmfix*, scmval**);
static scmval define_closure(scmfix, scmval*, scmval);
static scmval define_symbol(scmfix, scmval*, scmval);
static scmval eval_subr(scmval, scmfix, scmval*, scmval);
static scmval eval_closure(scmval, scmfix, scmval*, scmval);
static scmval eval_lambda(scmfix, scmval*, scmval);

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void init_eval() {
    scm_apply   = intern("apply");
    scm_define  = intern("define");
    scm_if      = intern("if");
    scm_set     = intern("set!");
    scm_lambda  = intern("lambda");
    scm_begin   = intern("begin");
}

#define dbg(P,V) { printf(">>> " P ": '"); write(scm_current_output_port(), V, scm_mode_write); printf("'\n"); }
#define COND if(false)
#define CASE(SYMBOL) else if(is_eq(s, SYMBOL))
#define DEFAULT else

////////////////////////////////////////////////////////////////////////////////
// E V A L U A T I O N
////////////////////////////////////////////////////////////////////////////////
scmval eval(scmval v, scmval e) {
    scmval r = v;
loop:
    if(is_symbol(v)) {
        r = lookup(e, v);
        if(is_undef(r))
            error(intern("error"), "undefined symbol '%s'", string_value(v));
    } else if(is_pair(v) && !is_null(v)) {
        scmfix  argc;
        scmval* argv;
        list_to_args(cdr(v), &argc, &argv);
        scmval s = car(v);
        COND { }
        CASE(scm_define) {
            if(is_pair(argv[0])) { // (define (name <args>)...
                r = define_closure(argc, argv, e);
            } else { // (define name...
                r = define_symbol(argc, argv, e);
            }
        }
        CASE(scm_lambda) {
            r = eval_lambda(argc, argv, e);
        }
        CASE(scm_if) {
            scmval test = eval(argv[0], e);
            v = is_true(test) ? argv[1] : argv[2];
            goto loop;
        } 
        CASE(scm_begin) {
            if(argc == 0) {
                r = scm_void;
            } else {
                for(int i = 0; i < argc - 1; i++)
                    eval(argv[i], e);
                v = argv[argc - 1];
                goto loop;
            }
        }
        CASE(scm_quote) {
            r = argv[0];
        }
        CASE(scm_apply) {
            v = cons(argv[0], eval(argv[1], e));
            goto loop;
        }
        DEFAULT {
            scmval f = is_callable(s) ? s : eval(s, e);
            if(is_subr(f)) {
                r = eval_subr(f, argc, argv, e);
            } else if(is_closure(f)) {
                r = eval_closure(f, argc, argv, e);
            }
        }
    } else { // immediate
        r = v;
    }
    return r;
}

static scmval eval_subr(scmval s, scmfix argc, scmval* argv, scmval e) {
    scmval r = scm_undef;
    check_arity(s, argc);
    scmfix new_argc = argc_from_arity(s, argc);
    if(argc > 0) {
        scmval* new_argv = scm_new_array(new_argc, scmval);
        for(int i = 0; i < new_argc; i++) {
            new_argv[i] = (i < argc)
                ? eval(argv[i], e)
                : scm_undef;
        }
        r = apply_funcall(s, new_argc, new_argv);
    } else {
        r = funcall0(s);
    }
    return r;
}

static scmval eval_closure(scmval f, scmfix argc, scmval* argv, scmval e) {
    scmfix ac = closure_argc(f);
    if(ac != argc) 
        error(arity_error_type, "%s expect %d arguments but received %d", 
                                is_undef(closure_name(f)) ? "anonymous closure" : string_value(closure_name(f)),
                                ac, argc);
    scmval *av = closure_argv(f);
    scmval env = make_env(closure_env(f));
    for(int i = 0; i < argc; i++) {
        scmval arg = eval(argv[i], e);
        bind(env, av[i], arg);
    }
    return eval(closure_body(f), env);
}

static scmval eval_lambda(scmfix argc, scmval* argv, scmval e) {
    if(argc < 2) error(arity_error_type, "lambda expects at least 2 arguments but received %d", argc);
    if(!is_pair(argv[0])) error(type_error_type, "unimplemented varargs lambda");
    scmfix ac;
    scmval* av;
    list_to_args(argv[0], &ac, &av);
    check_args("lambda", symbol_c, ac, av);
    scmval body = scm_null;
    for(int i = argc - 1; i >= 1; i--) {
        body = cons(argv[i], body);
    }
    body = cons(scm_begin, body);
    return make_closure(scm_undef, ac, av, make_env(e), body);
}

static scmval define_closure(scmfix argc, scmval* argv, scmval e) {
    if(argc != 2) error(arity_error_type, "define expects 2 arguments but received %d", argc);
    scmval  name = car(argv[0]);
    scmval  args = cdr(argv[0]);
    scmval  body = argv[1];
    scmfix  ac   = list_length(args);
    scmval* av = NULL;
    scmfix  i = 0;
    if(ac > 0) { 
        av = scm_new_array(ac, scmval);
        for(scmval l = args; !is_null(l); l = cdr(l)) {
            check_arg("define", symbol_c, car(l));
            av[i++] = car(l);
        }
    }
    scmval c = make_closure(make_string(string_value(name)), ac, av, make_env(e), body);
    dict_set(scm_context.globals, name, c);
    return scm_undef;
}

static scmval define_symbol(scmfix argc, scmval* argv, scmval e) {
    if(argc != 2) error(arity_error_type, "define expects 2 arguments but received %d", argc);
    scmval name = argv[0];
    scmval body = eval(argv[1], e);
    dict_set(scm_context.globals, name, body);
    if(is_closure(body))
        set_closure_name(body, name);
    return scm_undef;
}

static void list_to_args(scmval l, scmfix* argc, scmval** argv) {
    scmfix ac = list_length(l);
    if(ac > 0) {
        scmfix  i  = 0;
        scmval *av = scm_new_array(ac, scmval);
        for( ; !is_null(l); l = cdr(l)) {
            av[i++] = car(l);
        }
        *argv = av;
    }
    *argc = ac;
}

