#include "scm.h"

static scmval eval_aux(scmval, scmval);
static scmval stx_define(scmval, scmval);
static scmval stx_define_syntax(scmval, scmval);
static scmval stx_define_library(scmval, scmval);
static scmval stx_import(scmval, scmval);
static scmval stx_set(scmval, scmval);
static scmval stx_if(scmval, scmval);
static scmval stx_lambda(scmval, scmval);
static scmval stx_begin(scmval, scmval);
static scmval stx_quasiquote(scmval, scmval);
static scmval call_primitive(scmval, scmval, scmval);
static scmval call_closure(scmval, scmval, scmval);
static void   list_to_args(scmval, int*, scmval**);

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_eval(scmval expr, scmval env) {
    check_arg("eval", env_c, env);
    return eval(expr, env);
}

static scmval scm_void_(int argc, scmval *argv) {
    return scm_void;
}

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void init_eval(scmval env) {
    define(env, "eval", scm_eval,  arity_exactly(2));
    define(env, "void", scm_void_, arity_at_least(0));
}

////////////////////////////////////////////////////////////////////////////////
// E V A L U A T I O N
////////////////////////////////////////////////////////////////////////////////
scmval eval(scmval expr, scmval env) {
    expr = expand(expr, env);
    return eval_aux(expr, env);
}

#define COND if(false) {}
#define CASE(SYMBOL) else if(is_eq(s, SYMBOL))
#define DEFAULT else

static scmval eval_aux(scmval v, scmval e) {
    scmval r = v;
loop:
    //dbg("eval", v);
    if(is_null(v)) {
        error(syntax_error_type, "unquoted empty list is not a valid expression");
    } else if(is_symbol(v)) {
        r = lookup(e, v);
        if(is_undef(r))
            error(intern("error"), "undefined symbol '%s'", c_str(v));
    } else if(is_pair(v)) {
        scmval s = car(v);
        COND 
        CASE(sym_define_library) {
            r = stx_define_library(cdr(v), e);
        }
        CASE(sym_define) {
            r = stx_define(cdr(v), e);
        }
        CASE(sym_define_syntax) {
            r = stx_define_syntax(cdr(v), e);
        }
        CASE(sym_set) {
            r = stx_set(cdr(v), e);
        }
        CASE(sym_lambda) {
            r = stx_lambda(cdr(v), e);
        }
        CASE(sym_if) {
            v = stx_if(cdr(v), e);
            goto loop;
        }
        CASE(sym_begin) {
            v = stx_begin(cdr(v), e);
            goto loop;
        }
        CASE(sym_import) {
            r = stx_import(cdr(v), e);
        }
        CASE(sym_quote) {
            r = cadr(v);
        }
        CASE(sym_quasiquote) {
            r = stx_quasiquote(cdr(v), e);
        }
        CASE(sym_unquote) {
            error(syntax_error_type, "unquote: not in quasiquote");
        }
        CASE(sym_unquote_splicing) {
            error(syntax_error_type, "unquote-splicing: not in quasiquote");
        }
        CASE(sym_letrec_star) { // needed here for internal define
            v = expand(v, e);
            goto loop;
        }
        CASE(sym_apply) {
            scmval args = eval_aux(caddr(v), e);
            scmval head = scm_null, tail;
            foreach(arg, args) {
                if(is_null(head)) {
                    head = tail = cons(cons(sym_quote, cons(arg, scm_null)), scm_null);
                } else {
                    setcdr(tail, cons(cons(sym_quote, cons(arg, scm_null)), scm_null));
                    tail = cdr(tail);
                }
            }
            v = cons(cadr(v), head);
            //v = cons(cadr(v), eval(caddr(v), e));
            goto loop;
        }
        DEFAULT {
            scmval f = is_callable(car(v)) ? car(v) : eval_aux(car(v), e);
            if(is_primitive(f)) {
                r = call_primitive(f, cdr(v), e);
            } else if(is_closure(f)) {
                v = call_closure(f, cdr(v), e);
                // result is a cons (<env> . <tailexpr>)
                e = car(v);
                v = cdr(v);
                goto loop;
            } else if(is_continuation(f)) {
                r = eval_aux(cadr(v), e);
                set_continuation_value(f, r);
                call_continuation(f);
            } else if(is_syntax(f)) {
                v = expand(f, v);
                dbg("E(v)", v);
                goto loop;
            }
        }
    } else { // immediate
        r = v;
    }
    return r;
}

static scmval call_primitive(scmval prim, scmval arglist, scmval env) {
    scmval r = scm_undef;
    int     argc;
    scmval* argv;
    list_to_args(arglist, &argc, &argv);
    check_arity(prim, argc);
    int new_argc = argc_from_arity(prim, argc);
    scmval *new_argv = argv;
    if(new_argc > 0) {
        new_argv = scm_new_array(new_argc, scmval);
        for(int i = 0; i < new_argc; i++) {
            new_argv[i] = (i < argc)
                ? eval_aux(argv[i], env)
                : scm_undef;
        }
    }
    if(primitive_arity(prim).type == ARITY_AT_LEAST) {
        r = funcall(prim, new_argc, new_argv);
    } else {
        switch(new_argc) {
            case  0: r = funcall0(prim); break;
            case  1: r = funcall(prim, new_argv[0]); break;
            case  2: r = funcall(prim, new_argv[0], new_argv[1]); break;
            case  3: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2]); break;
            case  4: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3]); break;
            case  5: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4]); break;
            case  6: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4], new_argv[5]); break;
            case  7: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4], new_argv[5],
                                       new_argv[6]); break;
            case  8: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4], new_argv[5],
                                       new_argv[6], new_argv[7]); break;
            case  9: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4], new_argv[5],
                                       new_argv[6], new_argv[7], new_argv[8]); break;
            case 10: r = funcall(prim, new_argv[0], new_argv[1], new_argv[2],
                                       new_argv[3], new_argv[4], new_argv[5],
                                       new_argv[6], new_argv[7], new_argv[8],
                                       new_argv[9]); break;
        }
    }
    return r;
}

static scmval call_closure(scmval closure, scmval arglist, scmval e) {
    int     argc;
    scmval* argv;
    list_to_args(arglist, &argc, &argv);
    int ac = closure_argc(closure);
    scmval *av = closure_argv(closure);
    scmval env = make_env(closure_env(closure));
    if(ac == -1) { // lambda arg
        scmval arglist = scm_null;
        for(int i = argc - 1; i >= 0; i--) {
            scmval arg = eval_aux(argv[i], e);
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
            scmval arg = eval_aux(argv[i], e);
            bind(env, av[i], arg);
        }
        scmval tail = scm_null;
        for(int j = argc - 1; j >= xac; j--) {
            scmval arg = eval_aux(argv[j], e);
            tail = cons(arg, tail);
        }
        bind(env, av[i], tail);
    } else { // lambda (<arg>...)
        if(ac != argc) 
            error(arity_error_type, "%s expect %d arguments but received %d", 
                    is_undef(closure_name(closure)) ? "anonymous closure" : c_str(closure_name(closure)),
                    ac, argc);
        for(int i = 0; i < argc; i++) {
            scmval arg = eval_aux(argv[i], e);
            bind(env, av[i], arg);
        }
    }
    for(scmval exprs = closure_body(closure); !is_null(exprs); exprs = cdr(exprs)) {
        scmval expr = car(exprs);
        if(is_null(cdr(exprs)))
            return cons(env, expr);
        eval_aux(expr, env);
    }
    return scm_undef; // not reached
}

// Convert internal definitions to a letrec* form
static inline scmval transform_internal_definitions(scmval body) {
    bool   done = false;
    scmval defs = scm_null, td;
    scmval rest = scm_null, tr;
    foreach(expr, body) {
        if(is_list(expr) && is_eq(car(expr), sym_define)) {
            if(done) error(syntax_error_type, "internal definitions must appear at the beginning of the body");
            if(is_null(defs)) {
                defs = td = cons(cdr(expr), scm_null);
            } else {
                setcdr(td, cons(cdr(expr), scm_null));
                td = cdr(td);
            }
        } else {
            done = true;
            if(is_null(defs)) // no internal definitions found
                return body;
            if(is_null(rest)) {
                rest = tr = cons(expr, scm_null);
            } else {
                setcdr(tr, cons(expr, scm_null));
                tr = cdr(tr);
            }
        }
    }
    if(is_null(rest))
        rest = list1(scm_void);
    scmval result = cons(cons(sym_letrec_star, cons(defs, rest)), scm_null);
    return result;
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
    scmval body = transform_internal_definitions(cdr(expr));
    return make_closure(scm_undef, ac, av, env, body);
}

// FIXME duplicate stx_lambda
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
    body = transform_internal_definitions(body);
    scmval c = make_closure(s_str(c_str(name)), ac, av, env, body);
    set(env, name, c);
}

static void define_symbol(scmval expr, scmval e) {
    int len = list_length(expr);
    if(len != 2) error(arity_error_type, "define expects 2 arguments but received %d", len);
    scmval name = car(expr);
    scmval body = eval_aux(cadr(expr), e);
    set(e, name, body);
    if(is_closure(body)) {
        set_closure_name(body, name);
    }
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
    if(!is_eq(car(rule_list), sym_syntax_rules))
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
        if(is_eq(car(obj), sym_export)) {
            exports = cdr(obj);
        } else if(is_eq(car(obj), sym_import)) {
            imports = cons(cadr(obj), imports);
        } else if(is_eq(car(obj), sym_include)) {
            includes = cons(cdr(obj), includes);
        } else if(is_eq(car(obj), sym_begin)) {
            body = cdr(obj);
        }
    }
    int argc = list_length(imports), i = 0;
    scmval* argv = scm_new_array(argc, scmval);
    foreach(name, imports) {
        argv[i++] = name;
    }
    scmval libenv = scm_environment(argc, argv);
    get_env(libenv)->next = env; // XXX: is that the proper solution ?
    foreach(expr, body) {
        eval_aux(expr, libenv);
    }
    scmval library = make_library(name);
    dict_copy(library_symbols(library), env_globals(libenv));
    foreach(export, exports) {
        library_add_export(library, export);
    }
    register_library(library);
    return library;
}

static scmval stx_import(scmval expr, scmval env) {
    foreach(name, expr) {
        import(name, env);
    }
    return scm_undef;
}

static scmval stx_set(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len != 2) error(syntax_error_type, "invalid set! syntax");
    if(!is_symbol(car(expr)))
        error(syntax_error_type, "set!: expected symbol but got %s", scm_to_cstr(car(expr)));
    if(!update(env, car(expr), eval_aux(cadr(expr), env)))
        error(syntax_error_type, "set!: symbol %s is not defined", scm_to_cstr(car(expr)));
    return scm_void;
}

static scmval quasiquote(scmval expr, scmval env) {
    if(is_null(expr) || !is_pair(expr))
        return expr;
    scmval result = scm_null;
    if(is_eq(car(expr), sym_unquote)) {
        result = eval_aux(cadr(expr), env);
    } else if(is_pair(car(expr)) && is_eq(caar(expr), sym_unquote_splicing)) {
        scmval rest = cdr(expr);
        scmval tail = scm_null;
        expr = car(expr);
        result = eval_aux(cadr(expr), env);
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
    scmval test = eval_aux(car(expr), env);
    scmval consequent = cadr(expr);
    scmval alternate  = (len == 3) ? caddr(expr) : scm_void;
    return !is_false(test) ? consequent : alternate;
}

static scmval stx_begin(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len == 0)
        return scm_void;
    scmval lst = expr;
    for(lst = expr; !is_null(cdr(lst)); lst = cdr(lst)) {
        eval(car(lst), env);
    }
    return car(lst);
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

