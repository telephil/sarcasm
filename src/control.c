#include "scm.h"

scmval scm_procedure_p(scmval obj) {
    return s_bool(is_procedure(obj));
}

void init_control(scmval env) {
    define(env, "procedure?", scm_procedure_p, arity_exactly(1));
}

