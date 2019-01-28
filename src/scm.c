#include <readline/readline.h>
#include <readline/history.h>
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
static void init_readline(scmval env);

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
    init_foreign(env);
    init_library(env);
    init_env(env);
    // load scheme defined procedures / syntax
    with_error_handler(fatal_error_handler) {
        load("./lib/sarcasm/init.scm", env);
    }
    // TEMP
    init_readline(env);
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
    scm_display(err, scm_current_error_port());
    scm_display(s_char('\n'), scm_current_error_port());
    exit(1);
}

////////////////////////////////////////////////////////////////////////////////
// READLINE LIBRARY
// XXX will be moved to its own lib when ffi will be implemented
////////////////////////////////////////////////////////////////////////////////
static scmval scm_readline(scmval prompt) {
    check_arg("readline", string_c, prompt);
    char* line = readline(c_cstr(prompt));
    if(line == NULL)
        return scm_false;
    scmval s = s_str(line);
    free(line);
    return s;
}

static scmval scm_read_history(scmval filename) {
    check_arg("read-history", string_c, filename);
    read_history(c_cstr(filename));
    return scm_void;
}

static scmval scm_write_history(scmval filename) {
    check_arg("write-history", string_c, filename);
    write_history(c_cstr(filename));
    return scm_void;
}

static scmval scm_add_history(scmval history) {
    check_arg("add-history", string_c, history);
    add_history(c_cstr(history));
    return scm_void;
}

static scmval scm_generator;
static char** scm_completion_list_init(const char* text) {
    scmval ret = call(scm_generator, list1(s_str(text)));
    if(is_null(ret))
        return NULL;
    int l = list_length(ret);
    char** completions = calloc(l+1, sizeof(char*));
    completions[l] = NULL;
    int i = 0;
    foreach(s, ret) {
        completions[i++] = strdup(c_cstr(s));
    }
    return completions;
}

char* scm_completion_generator(const char* text, int state) {
    static char** completions = NULL;
    static int list_index, len;
    char *name;
    if(!state) {
        if(completions != NULL)
            free(completions);
        completions = scm_completion_list_init(text);
        if(completions == NULL)
            return NULL;
        list_index = 0;
        len = strlen(text);
    }
    while((name = completions[list_index++])) {
        if(!strncmp(name, text, len))
            return name;
    }
    return NULL;
}

static char** scm_completion_function(const char* text, int start, int end) {
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, scm_completion_generator);
}

static scmval scm_set_completion_function(scmval func) {
    scm_generator = func;
    rl_attempted_completion_function = scm_completion_function;
    return scm_void;
}

static scmval exit_hooks;

void run_exit_hooks() {
    foreach(hook, exit_hooks) {
        call(hook, scm_null);
    }
}

scmval scm_register_exit_hook(scmval proc) {
    push(proc, exit_hooks);
    return scm_void;
}

scmval scm_system(scmval command) {
    check_arg("system", string_c, command);
    int ret = system(c_cstr(command));
    if(ret == -1 || ret == 127)
        return scm_false;
    return s_fix(ret);
}

static void init_readline(scmval env) {
    define(env, "readline", scm_readline, arity_exactly(1));
    define(env, "read-history", scm_read_history, arity_exactly(1));
    define(env, "write-history", scm_write_history, arity_exactly(1));
    define(env, "add-history", scm_add_history, arity_exactly(1));
    define(env, "set-completion-function!", scm_set_completion_function, arity_exactly(1));
    define(env, "register-exit-hook", scm_register_exit_hook, arity_exactly(1));
    define(env, "system", scm_system, arity_exactly(1));
    exit_hooks = scm_null;
    atexit(run_exit_hooks);
}



