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
static void init_base_types();
static void save_command_line(int, char*[]);
static void init_features();
static void fatal_error_handler(scmval err);

////////////////////////////////////////////////////////////////////////////////
// I N I T I A L I Z A T I O N
////////////////////////////////////////////////////////////////////////////////
void scm_boot(int argc, char* argv[]) {
    GC_INIT();
    mp_set_memory_functions(scm_gc_malloc, scm_gc_realloc_z, scm_gc_free_z);

    init_base_types();

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
    init_foreign(env);
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
static void init_base_types() {
    register_type("#undef");
    register_type("#void");
    register_type("#null");
    register_type("#eof");
    register_type("#bool");
    register_type("#fixnum");
    register_type("#bignum");
    register_type("#flonum");
    register_type("#char");
    register_type("#string");
    register_type("#symbol");
    register_type("#pair");
    scm_type_vector = register_type("#vector");
    scm_type_bytevector = register_type("#bytevector");
    register_type("#environment");
    register_type("#primitive");
    register_type("#closure");
    register_type("#continuation");
    register_type("#parameter");
    register_type("#syntax");
    register_type("#error");
    register_type("#port");
    register_type("#library");
    register_type("#record");
}

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
    scm_display(err, scm_current_error_port());
    scm_display(s_char('\n'), scm_current_error_port());
    exit(1);
}

