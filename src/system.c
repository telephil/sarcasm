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
void load(const char* path, scmval env) {
    scmval p = scm_open_input_file(scm_str(path));
    while(true) {
        scmval v = read(p);
        if(is_eof(v))
            break;
        eval(v, env);
    }
    scm_close_input_port(p);
}

static void set_features() {
    short word = 0x0001;
    char* b    = (char*)&word;
    bool le    = (b[0] == 1);

    features = 
        cons(intern("r7rs"),
          cons(intern(PLATFORM),
            cons(intern(le ? "little-endian" : "big-endian"),
              cons(intern(IMPLEMENTATION_NAME),
                cons(intern(IMPLEMENTATION_NAME "-" IMPLEMENTATION_VERSION),
                  scm_null)))));
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
    opt_arg(env, scm_context.toplevel);
    check_arg("load", string_c, filename);
    check_arg("load", env_c, env);
    load(c_str(filename), env);
    return scm_undef;
}

// taken from unistd.h that we cannot include as we define conflicting versions
// of read and write functions
extern int access(const char*, int);

static scmval scm_file_exists_p(scmval filename) {
    check_arg("file-exists?", string_c, filename);
    return scm_bool(access(c_str(filename), 0) != -1);
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
    flonum res = 37.0 + ts.tv_sec + 0.000000001*ts.tv_nsec;
    return scm_flo(res);
}

static scmval scm_features() {
    return features;
}

void init_system(int argc, char* argv[]) {
    define("load",                       scm_load,           arity_or(1, 2));
    define("file-exists?",               scm_file_exists_p,  arity_exactly(1));
    define("delete-file",                scm_delete_file,    arity_exactly(1));
    define("command-line",               scm_command_line,   arity_exactly(0));
    define("exit",                       scm_exit,           arity_or(0, 1));
    define("emergency-exit",             scm_emergency_exit, arity_or(0, 1));
    define("current-second",             scm_current_second, arity_exactly(0));
    define("features",                   scm_features,       arity_exactly(0));
    define("get-environment-variable",   scm_getenv_var,     arity_exactly(1));
    define("get-environment-variables",  scm_getenv_vars,    arity_exactly(0));

    set_features();
    set_commandline(argc, argv);
}

