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
typedef bool(*contain_pred)(scmval, scmval);

static inline bool is_ellipsis(scmval v) { return is_eq(v, sym_ellipsis); }
static inline bool is_ellipsis_pair(scmval v) { return is_list(v) && is_ellipsis(car(v)); }

static inline bool is_ellipsis_var(scmval x, scmval ellipsis_vars) { return memq(x, ellipsis_vars); }
static inline bool is_not_literal(scmval x, scmval literals) { return !memq(x, literals); }
static inline bool is_not_binding(scmval x, scmval bindings) { return is_false(assq(x, bindings)); }

static inline scmval get_identifiers(scmval from, contain_pred pred, bool include_scalars, scmval x, scmval l) {
    if(is_symbol(x)) {
        if(include_scalars && pred(x, from))
            l = cons(x, l);
    } else if(is_list(x)) {
        if(is_ellipsis_pair(cdr(x))) {
            l = get_identifiers(from, pred, include_scalars, cddr(x), l);
            l = get_identifiers(from, pred, true, car(x), l);
        } else {
            l = get_identifiers(from, pred, include_scalars, cdr(x), l);
            l = get_identifiers(from, pred, include_scalars, car(x), l);
        }
    }
    return l;
}

static scmval match(scmval literals, scmval pattern, scmval expr, scmval bindings) {
    if(is_null(pattern)) {
        return is_null(expr) ? bindings : scm_false;
    } else if(is_symbol(pattern)) {
        if(memq(pattern, literals))
            return is_eq(pattern, expr) ? bindings : scm_false;
        push(cons(pattern, expr), bindings);
        return bindings;
    } else if(!is_list(pattern)) {
        return is_equal(pattern, expr) ? bindings : scm_false;
    } else if(is_ellipsis_pair(cdr(pattern))) {
        if(!is_pair(expr)) return scm_false;
        int tail_len  = list_length(cddr(pattern));
        int expr_len  = list_length(expr);
        int seq_len   = expr_len - tail_len;
        if(seq_len < 0) return scm_false;
        scmval expr_tail = list_tail(expr, seq_len);
        scmval seq = list_reverse(list_tail(list_reverse(expr), tail_len));
        scmval vars = get_identifiers(literals, is_not_literal, true, car(pattern), scm_null);
        scmval mseq = vars;
        scmval tally = scm_null;
        for(tally = mseq; !is_null(cdr(tally)); tally = cdr(tally)) {}
        foreach(elt, seq) {
            scmval v = map1(cdr, match(literals, car(pattern), elt, scm_null));
            setcdr(tally, v);
            tally = cdr(tally);
        }
        scmval rest = match(literals, cddr(pattern), expr_tail, bindings);
        scmval tail = rest;
        for(tail = rest; !is_null(cdr(tail)); tail = cdr(tail)) {}
        setcdr(tail, list1(mseq));
        return rest;
    } else if(is_list(expr)) {
        if(!is_list(pattern))
            return scm_false;
        scmval h = match(literals, car(pattern), car(expr), bindings);
        if(is_false(h)) return scm_false;
        scmval t = match(literals, cdr(pattern), cdr(expr), h); 
        return t;
    }
    return scm_false;
}

static scmval expand_part(scmval literals, scmval pattern, scmval template, scmval bindings, scmval evars) {
    scmval result = template;
    if(is_symbol(template)) {
        scmval res = assq(template, bindings);
        if(is_false(res) && memq(template, literals))
            result = template;
        else
            result = cdr(res);
    } else if(is_list(template)) {
        if(is_ellipsis_pair(cdr(template))) {
            scmval vars = get_identifiers(evars, is_ellipsis_var, true, car(template), scm_null);
            scmval vals = scm_null, t = vals;
            foreach(elt, vars) {
                scmval v = list1(cdr(assq(elt, bindings)));
                if(is_null(vals)) {
                    vals = t = v;
                } else {
                    setcdr(t, v);
                    t = cdr(t);
                }
            }
            scmval expanded_vals = expand_part(literals, pattern, car(template), map2(cons, vars, vals), evars);
            result = expand_part(literals, pattern, cddr(template), bindings, evars);
            if(!is_null(result)) {
                scmval tail = scm_null;
                for(tail = result; !is_null(cdr(tail)); tail = cdr(tail)) {}
                setcdr(tail, expanded_vals);
            } else {
                result = expanded_vals;
            }
        } else {
            result = cons(expand_part(literals, pattern, car(template), bindings, evars),
                          expand_part(literals, pattern, cdr(template), bindings, evars));
        }
    }
    return result;
}

static scmval expand_template(scmval literals, scmval pattern, scmval template, scmval bindings) {
    scmval new_literals  = get_identifiers(bindings, is_not_binding, true, template, scm_null);
    scmval ellipsis_vars = get_identifiers(literals, is_not_literal, false, cdr(pattern), scm_null);
    return expand_part(new_literals, pattern, template, bindings, ellipsis_vars);
}


static scmval expand_syntax(scmval stx, scmval expr) {
    scmval lit = syntax_literals(stx);
    foreach(rule, syntax_rules(stx)) {
        scmval pat  = car(rule);
        scmval tmpl = cadr(rule);
        scmval ret  = match(lit, cdr(pat), cdr(expr), scm_null);
        if(!is_false(ret)) {
            scmval exp = expand_template(lit, pat, tmpl, ret);
            return exp;
        }
    }
    error(syntax_error_type, "could not find syntax rule to match arguments");
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

