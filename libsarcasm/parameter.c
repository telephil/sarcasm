#include "scm.h"

static scmval dynenv;

void dynenv_push_frame(scmval frame) {
    push(frame, dynenv);
}

void dynenv_pop_frame() {
    dynenv = cdr(dynenv);
}

////////////////////////////////////////////////////////////////////////////////
// CONSTRUCTOR
////////////////////////////////////////////////////////////////////////////////
scmval make_parameter(scmval init, scmval converter) {
    scm_parameter_t* param = scm_new(scm_parameter_t);
    if(is_undef(converter))
        param->value = init;
    else
        param->value = call(converter, list1(init));
    return make_ptr(SCM_TYPE_PARAMETER, param);
}

scmval parameter_value(scmval p) {
    if(!is_null(dynenv)) {
        foreach(frame, dynenv) {
            foreach(pair, frame) {
                if(is_eq(car(pair), p))
                    return cdr(pair);
            }
        }
    }
    return get_parameter(p)->value;
}

void set_parameter_value(scmval p, scmval v) {
    scm_parameter_t* param = get_parameter(p);
    if(is_undef(param->converter))
        param->value = v;
    else
        param->value = call(param->converter, list1(v));
}
////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
scmval scm_make_parameter(scmval init, scmval converter) {
    return make_parameter(init, converter);
}

////////////////////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////////////////////
void init_parameter(scmval env) {
    dynenv = scm_null;

    define(env, "make-parameter", scm_make_parameter, arity_or(1, 2));
}

