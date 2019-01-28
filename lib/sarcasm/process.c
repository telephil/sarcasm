#include <stdlib.h>
#include <scm.h>

static scmval exit_hooks;

static void run_exit_hooks() {
    foreach(hook, exit_hooks) {
        call(hook, scm_null);
    }
}

////////////////////////////////////////////////////////////////////////////////
// LIBRARY FUNCTIONS
////////////////////////////////////////////////////////////////////////////////
static scmval scm_register_exit_hook(scmval proc) {
    push(proc, exit_hooks);
    return scm_void;
}

static scmval scm_system(scmval command) {
    check_arg("system", string_c, command);
    int ret = system(c_cstr(command));
    if(ret == -1 || ret == 127)
        return scm_false;
    return s_fix(ret);
}

////////////////////////////////////////////////////////////////////////////////
// MODULE INITIALIZATION
////////////////////////////////////////////////////////////////////////////////
void init_module(scmval env) {
    define(env, "register-exit-hook", scm_register_exit_hook, arity_exactly(1));
    define(env, "system", scm_system, arity_exactly(1));
    exit_hooks = scm_null;
    atexit(run_exit_hooks);
}

