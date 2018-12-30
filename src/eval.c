#include "scm.h"

static scmval scm_apply;
static scmval scm_define;
static scmval scm_set;
static scmval scm_define_syntax;
static scmval scm_syntax_rules;
static scmval scm_lambda;
static scmval scm_if;
static scmval scm_set;
static scmval scm_begin;
static scmval scm_define_library;
static scmval scm_import;
static scmval scm_export;
static scmval scm_include;
static scmval app_error_type;

static void   list_to_args(scmval, int*, scmval**);
static scmval stx_define(scmval, scmval);
static scmval stx_define_syntax(scmval, scmval);
static scmval stx_define_library(scmval, scmval);
static scmval stx_set(scmval, scmval);
static scmval stx_if(scmval, scmval);
static scmval stx_lambda(scmval, scmval);
static scmval stx_quasiquote(scmval, scmval);
static scmval apply_subr(scmval, scmval, scmval);
static scmval apply_closure(scmval, scmval, scmval);

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_eval(scmval expr, scmval env) {
    check_arg("eval", env_c, env);
    return eval(expr, env);
}

static scmval scm_void_subr(int argc, scmval *argv) {
    return scm_void;
}

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void init_eval(scmval env) {
    scm_apply           = intern("apply");
    scm_define          = intern("define");
    scm_define_syntax   = intern("define-syntax");
    scm_syntax_rules    = intern("syntax-rules");
    scm_if              = intern("if");
    scm_set             = intern("set!");
    scm_lambda          = intern("lambda");
    scm_begin           = intern("begin");
    scm_define_library  = intern("define-library");
    scm_import          = intern("import");
    scm_export          = intern("export");
    scm_include         = intern("include");
    app_error_type      = intern("application-error");

    define(env, "eval", scm_eval,      arity_exactly(2));
    define(env, "void", scm_void_subr, arity_at_least(0));
}

#define COND if(false) {}
#define CASE(SYMBOL) else if(is_eq(s, SYMBOL))
#define DEFAULT else

////////////////////////////////////////////////////////////////////////////////
// E V A L U A T I O N
////////////////////////////////////////////////////////////////////////////////
scmval eval(scmval v, scmval e) {
    scmval r = v;
loop:
    if(is_null(v)) {
        error(syntax_error_type, "unquoted empty list is not a valid expression");
    } else if(is_symbol(v)) {
        r = lookup(e, v);
        if(is_undef(r))
            error(intern("error"), "undefined symbol '%s'", c_str(v));
    } else if(is_pair(v)) {
        scmval s = car(v);
        COND 
        CASE(scm_define_library) {
            r = stx_define_library(cdr(v), e);
        }
        CASE(scm_define) {
            r = stx_define(cdr(v), e);
        }
        CASE(scm_define_syntax) {
            r = stx_define_syntax(cdr(v), e);
        }
        CASE(scm_set) {
            r = stx_set(cdr(v), e);
        }
        CASE(scm_lambda) {
            r = stx_lambda(cdr(v), e);
        }
        CASE(scm_if) {
            v = stx_if(cdr(v), e);
            goto loop;
        }
        CASE(scm_quote) {
            r = cadr(v);
        }
        CASE(scm_quasiquote) {
            r = stx_quasiquote(cdr(v), e);
        }
        CASE(scm_unquote) {
            error(syntax_error_type, "unquote: not in quasiquote");
        }
        CASE(scm_unquote_splicing) {
            error(syntax_error_type, "unquote-splicing: not in quasiquote");
        }
        CASE(scm_apply) {
            v = cons(cadr(v), eval(caddr(v), e));
            goto loop;
        }
        DEFAULT {
            scmval f = is_callable(car(v)) ? car(v) : eval(car(v), e);
            if(is_subr(f)) {
                r = apply_subr(f, cdr(v), e);
            } else if(is_closure(f)) {
                v = apply_closure(f, cdr(v), e);
                // result is a cons (<env> . <tailexpr>)
                e = car(v);
                v = cdr(v);
                goto loop;
            } else if(is_syntax(f)) {
                v = expand(f, v);
                //dbg("E(v)", v);
                goto loop;
            }
        }
    } else { // immediate
        r = v;
    }
    return r;
}

static scmval apply_subr(scmval subr, scmval arglist, scmval env) {
    scmval r = scm_undef;
    int     argc;
    scmval* argv;
    list_to_args(arglist, &argc, &argv);
    check_arity(subr, argc);
    int new_argc = argc_from_arity(subr, argc);
    if(new_argc > 0) {
        scmval* new_argv = scm_new_array(new_argc, scmval);
        for(int i = 0; i < new_argc; i++) {
            new_argv[i] = (i < argc)
                ? eval(argv[i], env)
                : scm_undef;
        }
        r = apply_funcall(subr, new_argc, new_argv);
    } else {
        r = funcall0(subr);
    }
    return r;
}

static scmval apply_closure(scmval closure, scmval arglist, scmval e) {
    int     argc;
    scmval* argv;
    list_to_args(arglist, &argc, &argv);
    int ac = closure_argc(closure);
    scmval *av = closure_argv(closure);
    scmval env = make_env(closure_env(closure));
    if(ac == -1) { // lambda arg
        scmval arglist = scm_null;
        for(int i = argc - 1; i >= 0; i--) {
            scmval arg = eval(argv[i], e);
            arglist = cons(arg, arglist);
        }
        bind(env, av[0], arglist);
    } else if(ac < 0) { // lambda (<arg>... . <arg>)
        int i   = 0;
        int xac = abs(ac) - 1;
        if(argc < xac)
            error(arity_error_type, "%s expects at least %d arguments but received %d",
                    is_undef(closure_name(closure)) ? "anonymous closure" : c_str(closure_name(closure)),
                    xac, argc);
        for(i = 0; i < xac; i++) {
            scmval arg = eval(argv[i], e);
            bind(env, av[i], arg);
        }
        scmval tail = scm_null;
        for(int j = argc - 1; j >= xac; j--) {
            scmval arg = eval(argv[j], e);
            tail = cons(arg, tail);
        }
        bind(env, av[i], tail);
    } else { // lambda (<arg>...)
        if(ac != argc) 
            error(arity_error_type, "%s expect %d arguments but received %d", 
                    is_undef(closure_name(closure)) ? "anonymous closure" : c_str(closure_name(closure)),
                    ac, argc);
        for(int i = 0; i < argc; i++) {
            scmval arg = eval(argv[i], e);
            bind(env, av[i], arg);
        }
    }
    for(scmval exprs = closure_body(closure); !is_null(exprs); exprs = cdr(exprs)) {
        scmval expr = car(exprs);
        if(is_null(cdr(exprs)))
            return cons(env, expr);
        eval(expr, env);
    }
    return scm_undef; // not reached
}

static scmval stx_lambda(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2) error(arity_error_type, "lambda expects at least 2 arguments but received %d", len);
    scmval arglist = car(expr);
    int     ac;
    scmval* av;
    if(is_pair(arglist)) {
        list_to_args(arglist, &ac, &av); // FIXME dotted arg list
        check_args("lambda", symbol_c, ac, av);
    } else {
        check_arg("lambda", symbol_c, arglist);
        ac = -1;
        av = scm_new_array(1, scmval);
        av[0] = arglist;
    }
    scmval body = cdr(expr);
    return make_closure(scm_undef, ac, av, env, body);
}

static void define_closure(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2) error(arity_error_type, "define expects at least 2 arguments but received %d", len);
    scmval  name = caar(expr);
    scmval  args = cdar(expr); 
    scmval  body = cdr(expr);
    int     ac   = list_length(args);
    scmval* av = NULL;
    int     i = 0;
    if(ac > 0) { 
        av = scm_new_array(ac, scmval);
        for(scmval l = args; !is_null(l); l = cdr(l)) {
            check_arg("define", symbol_c, car(l));
            av[i++] = car(l);
        }
    }
    scmval c = make_closure(scm_str(c_str(name)), ac, av, env, body);
    set(env, name, c);
}

static void define_symbol(scmval expr, scmval e) {
    int len = list_length(expr);
    if(len != 2) error(arity_error_type, "define expects 2 arguments but received %d", len);
    scmval name = car(expr);
    scmval body = eval(cadr(expr), e);
    set(e, name, body);
    if(is_closure(body))
        set_closure_name(body, name);
}

static scmval stx_define(scmval expr, scmval env) {
    if(is_pair(car(expr))) { // (define (name <args>)...
        define_closure(expr, env);
    } else { // (define name...
        define_symbol(expr, env);
    }
    return scm_undef;
}

static scmval stx_define_syntax(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len != 2) error(arity_error_type, "define-syntax expects 2 arguments but received %d", len);
    scmval name = car(expr);
    scmval rule_list = cadr(expr);
    if(!is_pair(rule_list))
        error(syntax_error_type, "define-syntax: expected a list of syntax-rules but received %s", scm_to_cstr(rule_list));
    if(!is_eq(car(rule_list), scm_syntax_rules))
        error(syntax_error_type, "define-syntax: expected syntax-rules but got %s", scm_to_cstr(car(rule_list)));
    if(!is_pair(cadr(rule_list)))
        error(syntax_error_type, "define-syntax: expected a list of literals but got %s", scm_to_cstr(cadr(rule_list)));
    for(scmval rules = cddr(rule_list); !is_null(rules); rules = cdr(rules)) {
        if(!is_pair(car(rules)) || list_length(car(rules)) != 2)
            error(syntax_error_type, "define-syntax: syntax rule should be a list with two elements but got %s", scm_to_cstr(car(rules)));
    }
    scmval syntax = make_syntax(name, cadr(rule_list), cddr(rule_list));
    set(env, name, syntax);
    return scm_undef;
}

static scmval stx_define_library(scmval expr, scmval env) {
    scmval name = car(expr);
    if(!is_list(expr)) error(syntax_error_type, "define-library: %s is not a valid library name", scm_to_cstr(name));
    // XXX check identifier + numbers
    scmval exports  = scm_null;
    scmval imports  = scm_null;
    scmval includes = scm_null;
    scmval body     = scm_null;
    foreach(obj, cdr(expr)) {
        if(!is_list(obj))
            error(syntax_error_type, "define-library: %s is not a valid library declaration", scm_to_cstr(name));
        if(is_eq(car(obj), scm_export)) {
            exports = cdr(obj);
        } else if(is_eq(car(obj), scm_import)) {
            imports = cons(cadr(obj), imports);
        } else if(is_eq(car(obj), scm_include)) {
            includes = cons(cdr(obj), includes);
        } else if(is_eq(car(obj), scm_begin)) {
            body = cdr(obj);
        }
    }
    int argc = list_length(imports), i = 0;
    scmval* argv = scm_new_array(argc, scmval);
    foreach(import, imports) {
        argv[i++] = import;
    }
    scmval libenv = scm_environment(argc, argv);
    foreach(expr, body) {
        eval(expr, libenv);
    }
    scmval library = make_library(name);
    dict_copy(library_symbols(library), env_globals(libenv));
    foreach(export, exports) {
        library_add_export(library, export);
    }
    register_library(library);
    return library;
}


static scmval stx_set(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len != 2) error(syntax_error_type, "invalid set! syntax");
    if(!is_symbol(car(expr)))
        error(syntax_error_type, "set!: expected symbol but got %s", scm_to_cstr(car(expr)));
    if(is_undef(lookup(env, car(expr))))
        error(syntax_error_type, "set!: symbol %s is not defined", scm_to_cstr(car(expr)));
    set(env, car(expr), eval(cadr(expr), env));
    return scm_undef;
}

static scmval quasiquote(scmval expr, scmval env) {
    if(is_null(expr) || !is_pair(expr))
        return expr;
    scmval result = scm_null;
    if(is_eq(car(expr), scm_unquote)) {
        result = eval(cadr(expr), env);
    } else if(is_pair(car(expr)) && is_eq(caar(expr), scm_unquote_splicing)) {
        scmval rest = cdr(expr);
        scmval tail = scm_null;
        expr = car(expr);
        result = eval(cadr(expr), env);
        for(tail = result; !is_null(cdr(tail)); tail = cdr(tail)) {
        }
        rest = quasiquote(rest, env);
        if(is_null(result))
            return rest;
        setcdr(tail, rest);
    } else {
        scmval head = quasiquote(car(expr), env);
        scmval tail = quasiquote(cdr(expr), env);
        result = cons(head, tail);
    }
    return result;
}

static scmval stx_quasiquote(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len != 1) error(syntax_error_type, "invalid quasiquote syntax");
    return quasiquote(car(expr), env);
}

static scmval stx_if(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2 || len > 3) error(syntax_error_type, "invalid syntax %s", scm_to_cstr(expr));
    scmval test = eval(car(expr), env);
    scmval consequent = cadr(expr);
    scmval alternate  = (len == 3) ? caddr(expr) : scm_void;
    return !is_false(test) ? consequent : alternate;
}

static void list_to_args(scmval l, int* argc, scmval** argv) {
    int ac = list_length(l);
    if(ac > 0) {
        int     i  = 0;
        scmval *av = scm_new_array(ac, scmval);
        for( ; !is_null(l); l = cdr(l)) {
            av[i++] = car(l);
            if(!is_pair(cdr(l))) {
                av[i] = cdr(l);
                ac = -ac;
                break;
            }
        }
        *argv = av;
    }
    *argc = ac;
}

