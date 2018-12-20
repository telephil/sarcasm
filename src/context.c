#include <gc/gc.h>

#include "scm.h"

scm_ctx_t scm_context;

void init_context() {
    scm_context.symbols  = make_dict();
    scm_context.globals  = make_dict();
    scm_context.toplevel = make_env(scm_undef);
}

