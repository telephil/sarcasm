#include "scm.h"
#include <time.h>

////////////////////////////////////////////////////////////////////////////////
// Globals
////////////////////////////////////////////////////////////////////////////////
static scmval features;
static scmval commandline;

////////////////////////////////////////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////////////////////////////////////////
scmval load(const char* path, scmval env) {
    scmval r = scm_undef;
    scmval p = scm_open_input_file(scm_str(path));
    while(true) {
        scmval v = read(p);
        if(is_eof(v))
            break;
        r = eval(v, env);
    }
    scm_close_input_port(p);
    return r;
}

static void set_features() {
    int word  = 0x0001;
    int le    = *((char*)&word);

    features = list(intern("r7rs"), intern("exact-closed"), intern("ieee-float"), intern("posix"),
                    intern(OS), intern(ARCH), intern(le ? "little-endian" : "big-endian"),
                    intern(IMPLEMENTATION_NAME), intern(IMPLEMENTATION_NAME "-" IMPLEMENTATION_VERSION),
                    scm_null);
}

static void set_commandline(int argc, char* argv[]) {
    commandline = scm_null;
    for(int i = argc - 1; i >= 0; i--) {
        commandline = cons(scm_str(argv[i]), commandline);
    }
}

////////////////////////////////////////////////////////////////////////////////
// STANDARD LIBRARY
////////////////////////////////////////////////////////////////////////////////
static scmval scm_load(scmval filename, scmval env) {
    opt_arg(env, scm_interaction_environment());
    check_arg("load", string_c, filename);
    check_arg("load", env_c, env);
    load(c_str(filename), env);
    return scm_undef;
}

// taken from unistd.h that we cannot include as we define conflicting versions
// of read and write functions
extern int access(const char*, int);

scmval scm_file_exists_p(scmval filename) {
    check_arg("file-exists?", string_c, filename);
    return scm_bool(access(c_cstr(filename), 0) != -1);
}

static scmval scm_delete_file(scmval filename) {
    check_arg("delete-file", string_c, filename);
    return scm_bool(remove(c_str(filename)) == 0);
} 

static scmval scm_command_line() {
    return commandline;
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
    return scm_str(value);
}

static scmval scm_getenv_vars() {
    extern char** environ;
    scmval l = scm_null;
    for(char** env = environ; *env; env++) {
        char *s = strdup(*env);
        char *k = strtok(s, "=");
        char *v = strtok(NULL, "=");
        l = cons(cons(scm_str(k), scm_str(v)), l);
        free(s);
    }
    return l;
}

static scmval scm_current_second() {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    // TAI is 37s ahead of UTC (thanks Wikipedia)
    flonum res = 37 + ts.tv_sec + 0.000000001*ts.tv_nsec;
    return scm_flo(res);
}

static scmval scm_features() {
    return features;
}

void init_system(scmval env, int argc, char* argv[]) {
    define(env, "load",                       scm_load,           arity_or(1, 2));
    define(env, "file-exists?",               scm_file_exists_p,  arity_exactly(1));
    define(env, "delete-file",                scm_delete_file,    arity_exactly(1));
    define(env, "command-line",               scm_command_line,   arity_exactly(0));
    define(env, "exit",                       scm_exit,           arity_or(0, 1));
    define(env, "emergency-exit",             scm_emergency_exit, arity_or(0, 1));
    define(env, "current-second",             scm_current_second, arity_exactly(0));
    define(env, "features",                   scm_features,       arity_exactly(0));
    define(env, "get-environment-variable",   scm_getenv_var,     arity_exactly(1));
    define(env, "get-environment-variables",  scm_getenv_vars,    arity_exactly(0));

    set_features();
    set_commandline(argc, argv);
}

