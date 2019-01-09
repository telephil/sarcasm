#include "scm.h"

scmval syntax_error_type;

////////////////////////////////////////////////////////////////////////////////
// CONSTRUCTOR
////////////////////////////////////////////////////////////////////////////////
scmval make_syntax(scmval name, scmval literals, scmval rules) {
    scm_syntax_t* syntax = scm_new(scm_syntax_t);
    syntax->name        = name;
    syntax->literals    = literals;
    syntax->rules       = rules;
    return make_ptr(SCM_TYPE_SYNTAX, syntax);
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_expand(scmval expr) {
    return expand(expr, scm_interaction_environment());
}

////////////////////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////////////////////
void init_syntax(scmval env) {
    define(env, "expand", scm_expand, arity_exactly(1));
}

////////////////////////////////////////////////////////////////////////////////
// Forward declarations
////////////////////////////////////////////////////////////////////////////////
static scmval expand_syntax(scmval, scmval);
static scmval find_matching_rule(scmval, scmval);
static scm_dict_t* pattern_bind_vars(scmval, scmval);
static scmval template_instantiate(scmval, scmval, scm_dict_t*);
static scmval stx_case_lambda(scmval, scmval);
static scmval stx_let(scmval, scmval);
static scmval stx_let_star(scmval, scmval);
static scmval stx_letrec_star(scmval, scmval);
static scmval stx_define_record_type(scmval, scmval);

////////////////////////////////////////////////////////////////////////////////
// SYNTAX EXPANSION 
////////////////////////////////////////////////////////////////////////////////
scmval expand(scmval expr, scmval env) {
    if(!is_list(expr)) {
        return expr;
    }
#define maybe_expand(V,E) list1(is_list(V) ? expand(V, E) : V)
    scmval result = maybe_expand(car(expr), env);
    scmval tail   = result;
    if(is_list(cdr(expr))) {
        for(scmval l = cdr(expr); !is_null(l); l = cdr(l)) {
            scmval elt = car(l);
            setcdr(tail, maybe_expand(elt, env));
            tail = cdr(tail);
            if(!is_list(cdr(l))) {
                setcdr(tail, cdr(l));
                break;
            }
        }
    } else { // dotted list
        setcdr(result, cdr(expr));
    }
    scmval expanded = result;
    if(is_eq(car(result), sym_case_lambda)) {
        expanded = stx_case_lambda(cdr(result), env);
    } else if(is_eq(car(result), sym_let)) {
        expanded = stx_let(cdr(result), env);
    } else if(is_eq(car(result), sym_let_star)) {
        expanded = stx_let_star(cdr(result), env);
    } else if(is_eq(car(result), sym_letrec)) {
        expanded = stx_letrec_star(cdr(result), env);
    } else if(is_eq(car(result), sym_letrec_star)) {
        expanded = stx_letrec_star(cdr(result), env);
    } else if(is_eq(car(result), sym_define_record_type)) {
        expanded = stx_define_record_type(cdr(result), env);
    } else if(is_symbol(car(result))) {
        scmval v = lookup(env, car(result));
        if(is_syntax(v)) {
            expanded = expand_syntax(v, result);
        }
    }
    scmval previous = expr;
    while(!is_eq(previous, expanded) && !is_equal(previous, expanded)) {
        previous = expanded;
        expanded = expand(expanded, env);
        break;
    }
    //dbg("(expand) before", expr);
    //dbg("(expand)  after", expanded);
    return expanded;
}

////////////////////////////////////////////////////////////////////////////////
// SCHEME TRANSFORMER
////////////////////////////////////////////////////////////////////////////////
static scmval expand_syntax(scmval stx, scmval expr) {
    scmval rule = find_matching_rule(stx, expr);
    if(is_undef(rule))
        error(syntax_error_type, "could not find syntax rule to match arguments");
    scmval pattern  = car(rule);
    scmval template = cadr(rule);
    // apply template
    scmval result = scm_undef;
    // XXX is it really necessary ?
    if(!is_eq(car(pattern), sym_underscore) && !is_eq(car(pattern), car(expr))) {
        error(syntax_error_type, "pattern does not match expression %s", scm_to_cstr(car(expr)));
    }
    if(is_null(cdr(pattern))) {
        result = template;
    } else if(is_symbol(template)) {
        if(is_eq(cadr(pattern), template))
            result = cadr(expr);
        else
            result = template;
    } else if(is_pair(template)) {
        scm_dict_t* dict = pattern_bind_vars(pattern, cdr(expr));
        result = template_instantiate(stx, template, dict);
    } else {
        error(syntax_error_type, "unhandled template type %s", scm_to_cstr(template));
    }
    return result;
}

static scmval template_instantiate(scmval stx, scmval template, scm_dict_t* vars) {
    scmval head = scm_null, tail = scm_null;
    for(scmval tmpl = template; !is_null(tmpl); tmpl = cdr(tmpl)) {
        scmval expr = car(tmpl);
        if(is_symbol(expr)) {
            scmval val = dict_ref(vars, expr);
            if(!is_undef(val)) {
                if(!is_eq(expr, sym_ellipsis)) {
                    expr = cons(val, scm_null);
                } else {
                    expr = val;
                }
            } else {
                expr = cons(expr, scm_null);
            }
        } else if(is_pair(expr)) {
            expr = list1(template_instantiate(stx, expr, vars));
        } else {
            expr = cons(expr, scm_null);
        }
        if(is_null(head)) {
            head = tail = expr;
        } else {
            setcdr(tail, expr);
            tail = expr;
        }
    }
    return head;
}

static scm_dict_t* pattern_bind_vars(scmval pattern, scmval arglist) {
    scm_dict_t* dict = make_dict();
    for(scmval pvar = cdr(pattern); !is_null(pvar); pvar = cdr(pvar)) {
        scmval val = scm_null;
        if(is_eq(car(pvar), sym_ellipsis)) {
            if(!is_null(arglist))
                val = arglist;
        } else {
            val = car(arglist);
            arglist = cdr(arglist);
        }
        dict_set(dict, car(pvar), val);
    }
    return dict;
}

static bool pattern_match(scmval pattern, int argc) {
    scmval l = pattern;
    int  count = 0;
    bool more = false;
    while(!is_null(l)) {
        ++count;
        if(is_eq(car(l), sym_ellipsis))
            more = true;
        l = cdr(l);
    }
    if(more && argc >= (count - 1))
        return true;
    if(argc == count)
        return true;
    return false;
}

static scmval find_matching_rule(scmval syn, scmval expr) {
    int argc = list_length(expr);
    for(scmval rules = syntax_rules(syn); !is_null(rules); rules = cdr(rules)) {
        scmval rule = car(rules);
        scmval pattern = car(rule);
        if(pattern_match(pattern, argc))
            return rule;
    }
    return scm_undef;
}

////////////////////////////////////////////////////////////////////////////////
// NATIVE TRANSFORMERS
////////////////////////////////////////////////////////////////////////////////
// PLAIN UGLY C MACRO EXPANSION :(
static scmval stx_case_lambda(scmval expr, scmval env) {
    if(is_null(expr)) error(syntax_error_type, "invalid case-lambda syntax");
    // transform all clauses to if / apply
    int len = list_length(expr);
    scmval* transformed = scm_new_array(len+1, scmval);
    int i = 0;
    foreach(clause, expr) {
        scmval p = list3(sym_if,
                         list3(intern("="), intern("len"), s_fix(list_length(car(clause)))),
                         list3(sym_apply, cons(sym_lambda, clause), intern("args")));
        transformed[i++] = p;
    }
    transformed[i] = list2(intern("error"), s_str("no matching clause found"));
    scmval body = cons(transformed[0], scm_null);
    for(int i = 0; i < len; i++) {
        setcdr(cddr(transformed[i]), cons(transformed[i+1], scm_null));
    }
    scmval result =
        list3(sym_lambda, intern("args"),
              cons(sym_let,
                   cons(list1(list2(intern("len"), list2(intern("length"), intern("args")))),
                        body)));
    return result;
}

// (define-syntax let
//   (syntax-rules ()
//     ((let ((name val) ...) body1 body2 ...)
//       ((lambda (name ...) body1 body2 ...)
//         val ...))
//     ((let tag ((name val) ...) body1 body2 ...)
//       ((letrec ((tag (lambda (name ...)
//                        body1 body2 ...)))
//          tag)
//       val ...))))
static scmval stx_let(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2) error(syntax_error_type, "invalid let syntax: expected at least 2 arguments but received %d", len);
    bool named = is_symbol(car(expr));
    if(named && len < 3)
        error(syntax_error_type, "invalid let syntax: expected at least 3 arguments but received %", len);
    scmval name    = named ? car(expr) : scm_undef;
    scmval arglist = named ? cadr(expr) : car(expr);
    scmval body    = named ? cddr(expr) : cdr(expr);
    scmval vars    = scm_null;
    scmval vals    = scm_null;
    scmval tvars, pvars, tvals, pvals;
    foreach(arg, arglist) {
        if(!is_list(arg))
            error(syntax_error_type, "invalid let syntax: expected a list but received %s", scm_to_cstr(arg));
        pvars = cons(car(arg), scm_null);
        pvals = cons(cadr(arg), scm_null);
        if(is_null(vars)) {
            vars = tvars = pvars;
            vals = tvals = pvals;
        } else {
            setcdr(tvars, pvars);
            tvars = pvars;
            setcdr(tvals, pvals);
            tvals = pvals;
        }
    }
    scmval ldef = cons(sym_lambda, cons(vars, body)); // (lambda (vars...) body...)
    scmval result = scm_undef;
    if(named) {
        scmval lrdef = list3(sym_letrec, list1(cons(name, list1(ldef))), name);
        result = cons(lrdef, vals);
    } else {
        result = cons(ldef, vals); // (ldef values...)
    }
    return result;
}

// (define-syntax let*
//   (syntax-rules ()
//     ((let* () body1 body2 ...)
//       (let () body1 body2 ...))
//     ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
//      (let ((name1 val1))
//        (let* ((name2 val2) ...)
//          body1 body2 ...)))))
static scmval stx_let_star(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2) error(syntax_error_type, "invalid let* syntax");
    scmval arglist = car(expr);
    scmval body    = cdr(expr);
    if(is_null(arglist))
        return stx_let(expr, env);

    scmval rest   = cons(sym_let_star, cons(cdr(arglist), body));
    scmval result = cons(sym_let, cons(cons(car(arglist), scm_null), cons(rest, scm_null)));
    return result;
}

// (define-syntax letrec*
//   (syntax-rules ()
//     ((letrec* ((var1 init1) ...) body1 body2 ...)
//      (let ((var1 <undefined>) ...)
//        (set! var1 init1)
//        ...
//        (let () body1 body2 ...)))))
static scmval stx_letrec_star(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 2) error(syntax_error_type, "invalid letrec* syntax");
    scmval arglist = car(expr);
    scmval body    = cons(sym_let, cons(scm_null, cdr(expr)));
    scmval vars    = scm_null;
    scmval vals    = scm_null;
    scmval tvars, pvars, tvals, pvals;
    foreach(arg, arglist) {
        if(!is_list(arg))
            error(syntax_error_type, "invalid letrec* syntax: expected a list but received %s", scm_to_cstr(arg));
        pvars = list1(list2(car(arg), scm_void));    // ((var1 #undefined)...)
        pvals = list1(list3(sym_set, car(arg), cadr(arg))); // (set! var1 val1)...
        if(is_null(vars)) {
            vars = tvars = pvars;
            vals = tvals = pvals;
        } else {
            setcdr(tvars, pvars);
            tvars = pvars;
            setcdr(tvals, pvals);
            tvals = pvals;
        }
    }
    setcdr(tvals, cons(body, scm_null));
    scmval result = cons(sym_let, cons(vars, vals)); // (let (vars...) vals... (let () body...))
    return result;
}

// define-record
static scmval define_record_predicate(scmval pred, scmval type, scmval env) {
    scmval obj  = intern("obj");
    scmval body =
        list3(intern("and"), 
              list2(intern("%record?"), obj),
              list3(intern("eq?"), list2(intern("%record-type"), obj), list2(sym_quote, type)));
    scmval proc =
        list3(sym_define, pred, list3(sym_lambda, list1(obj), body));
    return proc;
}

static scmval codegen_record_field_accessor(scmval name, int index) {
    scmval obj  = intern("obj");
    scmval proc =
        list3(sym_define, name,
                list3(sym_lambda, list1(obj),
                    list3(intern("vector-ref"), list2(intern("%record-slots"), obj), s_fix(index))));
    return proc;
}

static scmval codegen_record_field_mutator(scmval name, int index) {
    scmval obj  = intern("obj");
    scmval val  = intern("val");
    scmval proc =
        list3(sym_define, name,
                list3(sym_lambda, list2(obj, val),
                    list4(intern("vector-set!"), list2(intern("%record-slots"), obj), s_fix(index), val)));
    return proc;
}

static scmval define_record_fields(scmval fields, scmval env) {
    scmval result = scm_null;
    int index = 0;
    foreach(field, fields) {
        int len = list_length(field);
        scmval mutator = scm_undef;
        scmval accessor = scm_undef;
        switch(len) {
            case 3:
                mutator = codegen_record_field_mutator(caddr(field), index);
            case 2:
                accessor = codegen_record_field_accessor(cadr(field), index);
                break;
            default:
                error(syntax_error_type, "invalid record field syntax");
        }
        index++;
        result = cons(accessor, result);
        if(!is_undef(mutator))
            result = cons(mutator, result);
    }
    return result;
}

static scmval define_record_ctor(scmval ctor, scmval type, scmval fields, scmval env) {
    scmval sdef = cons(intern("vector"), scm_null), t = sdef;
    foreach(field, fields) {
        scmval fname = car(field);
        bool   found = false;
        foreach(fref, cdr(ctor)) {
            if(is_eq(fref, fname)) {
                found = true;
                break;
            }
        }
        setcdr(t, list1(found ? fname : list1(sym_void)));
        t = cdr(t);
    }
    scmval proc =
        list3(sym_define, car(ctor),
            list3(sym_lambda, cdr(ctor),
                list3(intern("%make-record"), list2(sym_quote, type), sdef)));
    return proc;
}
static scmval stx_define_record_type(scmval expr, scmval env) {
    int len = list_length(expr);
    if(len < 4) error(syntax_error_type, "invalid define-record-type syntax");
    scmval type   = car(expr);
    scmval ctor   = cadr(expr);
    scmval pred   = caddr(expr);
    scmval fields = cdddr(expr);
    if(!is_symbol(type))
        error(syntax_error_type, "invalid record name (expected a symbol but got %s", scm_to_cstr(type)); 
    scmval code =
        cons(sym_begin,
                cons(define_record_ctor(ctor, type, fields, env),
                    cons(define_record_predicate(pred, type, env),
                        define_record_fields(fields, env))));
    return code;
}

