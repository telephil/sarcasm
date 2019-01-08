#include "scm.h"

////////////////////////////////////////////////////////////////////////////////
// GLOBALS
////////////////////////////////////////////////////////////////////////////////
scmval scm_undef;
scmval scm_void;
scmval scm_null;
scmval scm_true;
scmval scm_false;
scmval scm_eof;

////////////////////////////////////////////////////////////////////////////////
// FORWARD DECLARATIONS
////////////////////////////////////////////////////////////////////////////////
static void fatal_error_handler(scmval err);

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void scm_boot(int argc, char* argv[]) {
    GC_INIT();
    mp_set_memory_functions(scm_gc_malloc, scm_gc_realloc, scm_gc_free);

    // create globals
    scm_undef = make_val(SCM_TYPE_UNDEF);
    scm_void  = make_val(SCM_TYPE_VOID);
    scm_null  = make_val(SCM_TYPE_NULL);
    scm_true  = make_bool(true);
    scm_false = make_bool(false);
    scm_eof   = make_val(SCM_TYPE_EOF);

    // intern common symbols
    init_intern_pool();

    // register primitives
    scmval env = make_env(scm_undef);
    init_symbol(env);
    init_errors(env);
    init_bool(env);
    init_number(env);
    init_char(env);
    init_string(env);
    init_pair(env);
    init_vector(env);
    init_bytevector(env);
    init_port(env);
    init_syntax(env);
    init_reader(env);
    init_eval(env);
    init_system(env, argc, argv);
    init_control(env);
    init_record(env);
    init_library(env);
    init_env(env);
    // load scheme defined procedures / syntax
    with_error_handler(fatal_error_handler) {
        load("./lib/sarcasm/init.scm", env);
    }
    // create core library
    make_core_library(env);
    // then create standard environments
    post_init_env();
}

////////////////////////////////////////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////////////////////////////////////////
static void fatal_error_handler(scmval err) {
    write(scm_current_error_port(), err, scm_mode_display);
    write(scm_current_error_port(), s_char('\n'), scm_mode_display);
    exit(1);
}

