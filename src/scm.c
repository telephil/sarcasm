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

scmval scm_g_command_line;
scmval scm_g_features;

static const char* features[] = {
    "r7rs",
    "exact-closed",
    "ieee-float",
    "posix",
    OS,
    ARCH,
    ENDIANESS,
    IMPLEMENTATION_NAME,
    IMPLEMENTATION_NAME "-" IMPLEMENTATION_VERSION
};
static const int nfeatures = sizeof(features) / sizeof(features[0]);

////////////////////////////////////////////////////////////////////////////////
// FORWARD DECLARATIONS
////////////////////////////////////////////////////////////////////////////////
static void fatal_error_handler(scmval err);
static void save_command_line(int, char*[]);
static void init_features();

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

    save_command_line(argc, argv);
    init_features();

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
    init_system(env);
    init_control(env);
    init_parameter(env);
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
static void save_command_line(int argc, char* argv[]) {
    scm_g_command_line = scm_null;
    for(int i = argc - 1; i >= 0; i--) {
        push(s_str(argv[i]), scm_g_command_line);
    }
}

static void init_features() {
    scm_g_features = scm_null;
    for(int i = nfeatures - 1; i >= 0; i--) {
        push(intern(features[i]), scm_g_features);
    }
}

static void fatal_error_handler(scmval err) {
    write(scm_current_error_port(), err, scm_mode_display);
    write(scm_current_error_port(), s_char('\n'), scm_mode_display);
    exit(1);
}

