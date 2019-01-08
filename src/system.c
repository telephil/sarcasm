#include "scm.h"
#include <time.h>

////////////////////////////////////////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////////////////////////////////////////
scmval load(const char* path, scmval env) {
    scmval r = scm_undef;
    scmval p = scm_open_input_file(s_str(path));
    while(true) {
        scmval v = read(p);
        if(is_eof(v))
            break;
        r = eval(v, env);
    }
    scm_close_input_port(p);
    return r;
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_load(scmval filename, scmval env) {
    opt_arg(env, scm_interaction_environment());
    check_arg("load", string_c, filename);
    check_arg("load", env_c, env);
    load(c_str(filename), env);
    return scm_void;
}

// taken from unistd.h that we cannot include as we define conflicting versions
// of read and write functions
extern int access(const char*, int);

scmval scm_file_exists_p(scmval filename) {
    check_arg("file-exists?", string_c, filename);
    return s_bool(access(c_cstr(filename), 0) != -1);
}

static scmval scm_delete_file(scmval filename) {
    check_arg("delete-file", string_c, filename);
    return s_bool(remove(c_str(filename)) == 0);
} 

static scmval scm_command_line() {
    return scm_g_command_line;
}

static scmval scm_exit(scmval obj) {
    if(is_undef(obj))
        exit(0);
    if(is_fixnum(obj))
        exit(c_fix(obj));
    if(is_flonum(obj))
        exit((int)c_flo(obj));
    exit(is_false(obj));
    return scm_undef;
}

static scmval scm_emergency_exit(scmval obj) {
    if(is_undef(obj))
        _Exit(0);
    if(is_fixnum(obj))
        _Exit(c_fix(obj));
    if(is_flonum(obj))
        _Exit((int)c_flo(obj));
    _Exit(is_false(obj));
    return scm_undef;
}

static scmval scm_getenv_var(scmval name) {
    check_arg("get-environment-variable", string_c, name);
    char* value = getenv(c_str(name));
    if(value == NULL)
        return scm_false;
    return s_str(value);
}

static scmval scm_getenv_vars() {
    extern char** environ;
    scmval l = scm_null;
    for(char** env = environ; *env; env++) {
        char *s = strdup(*env);
        char *k = strtok(s, "=");
        char *v = strtok(NULL, "=");
        l = cons(cons(s_str(k), s_str(v)), l);
        free(s);
    }
    return l;
}

#define TAI_OFFSET 37
static scmval scm_current_second() {
    time_t t = time(NULL);
    return s_flo(t + TAI_OFFSET);
}

static scmval scm_current_jiffy() {
    clock_t jiffy = clock();
    return s_flo(jiffy);
}

static scmval scm_jiffies_per_second() {
    return s_flo(CLOCKS_PER_SEC);
}

static scmval scm_features() {
    return scm_g_features;
}

void init_system(scmval env, int argc, char* argv[]) {
    define(env, "load",                       scm_load,                 arity_or(1, 2));
    define(env, "file-exists?",               scm_file_exists_p,        arity_exactly(1));
    define(env, "delete-file",                scm_delete_file,          arity_exactly(1));
    define(env, "command-line",               scm_command_line,         arity_exactly(0));
    define(env, "exit",                       scm_exit,                 arity_or(0, 1));
    define(env, "emergency-exit",             scm_emergency_exit,       arity_or(0, 1));
    define(env, "current-second",             scm_current_second,       arity_exactly(0));
    define(env, "current-jiffy",              scm_current_jiffy,        arity_exactly(0));
    define(env, "jiffies-per-second",         scm_jiffies_per_second,   arity_exactly(0));
    define(env, "features",                   scm_features,             arity_exactly(0));
    define(env, "get-environment-variable",   scm_getenv_var,           arity_exactly(1));
    define(env, "get-environment-variables",  scm_getenv_vars,          arity_exactly(0));
}

