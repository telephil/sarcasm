#include "scm.h"

scmval scm_underscore;
scmval scm_ellipsis;
scmval syntax_error_type;

void init_syntax() {
    scm_underscore  = intern("_");
    scm_ellipsis    = intern("...");
    syntax_error_type = intern("syntax-error");
}

scmval make_syntax(scmval name, scmval literals, scmval rules) {
    scm_syntax_t* syntax = scm_new(scm_syntax_t);
    syntax->name        = name;
    syntax->literals    = literals;
    syntax->rules       = rules;
    return make_ptr(SCM_TYPE_SYNTAX, syntax);
}

static scmval find_matching_rule(scmval, scmval);
static scm_dict_t* pattern_bind_vars(scmval, scmval);
static scmval template_instantiate(scmval, scm_dict_t*);

scmval expand(scmval syn, scmval expr) {
    scmval rule = find_matching_rule(syn, expr);
    if(is_undef(rule))
        error(syntax_error_type, "could not find syntax rule to match arguments");
    scmval pattern  = car(rule);
    scmval template = cadr(rule);
    /*
    printf("#<rule: ");
    write(scm_current_output_port(), pattern, scm_mode_write);
    printf(" => ");
    write(scm_current_output_port(), template, scm_mode_write);
    printf(" >\n");
    */
    // apply template
    scmval result = scm_undef;
    // XXX is it really necessary ?
    if(!is_eq(car(pattern), scm_underscore) && !is_eq(car(pattern), car(expr))) {
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
        result = template_instantiate(template, dict);
    } else {
        error(syntax_error_type, "unhandled template type %s", scm_to_cstr(template));
    }
    return result;
}

static scmval template_instantiate(scmval template, scm_dict_t* vars) {
    scmval head = scm_null, tail = scm_null;
    for(scmval tmpl = template; !is_null(tmpl); tmpl = cdr(tmpl)) {
        scmval expr = car(tmpl);
        if(is_symbol(expr)) {
            scmval val = dict_ref(vars, expr);
            if(!is_undef(val)) {
                if(!is_eq(expr, scm_ellipsis)) {
                    expr = cons(val, scm_null);
                } else {
                    expr = val;
                }
            } else {
                expr = cons(expr, scm_null);
            }
        } else if(is_pair(expr)) {
            expr = cons(template_instantiate(expr, vars), scm_null);
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
        if(is_eq(car(pvar), scm_ellipsis)) {
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
        if(is_eq(car(l), scm_ellipsis))
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

