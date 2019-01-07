#include <readline/readline.h>
#include <readline/history.h>
#include <time.h>
#include "scm.h"

static void default_error_handler(scmval err);
static void repl();
static void run(const char*);

////////////////////////////////////////////////////////////////////////////////
// M A I N
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {
    scm_boot(argc, argv);
    if(argc == 2) {
        run(argv[1]);
    } else {
        printf("scm v0.1\n");
        repl();
        printf("Bye.\n");
    }
    return 0;
}

////////////////////////////////////////////////////////////////////////////////
// R U N  P R O G R A M
////////////////////////////////////////////////////////////////////////////////
void run(const char* filename) {
    scmval env = make_env(scm_undef);
    with_error_handler(default_error_handler) {
        load(filename, env);
    }
}

////////////////////////////////////////////////////////////////////////////////
// E R R O R  H A N D L E R S
////////////////////////////////////////////////////////////////////////////////
static void default_error_handler(scmval err) {
    write(scm_current_error_port(), err, scm_mode_display);
    write(scm_current_error_port(), s_char('\n'), scm_mode_display);
}

////////////////////////////////////////////////////////////////////////////////
// R E P L
////////////////////////////////////////////////////////////////////////////////
static char** proc_names;

static char* get_history_filename() {
    static const char* filename = ".scm_history";
    char *path;
    asprintf(&path, "%s/%s", getenv("HOME"), filename);
    return path;
}

static char* proc_name_generator(const char* text, int state) {
    static int list_index, len;
    char *name;
    if(!state) {
        list_index = 0;
        len = strlen(text);
    }
    while((name = proc_names[list_index++])) {
        if(!strncmp(name, text, len))
            return strdup(name);
    }
    return NULL;
}

static char** proc_name_completion_function(const char* text, int start, int end) {
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, proc_name_generator);
}

static void init_completion() {
    int     size;
    scmval* procs;
    dict_keys(env_globals(scm_interaction_environment()), &size, &procs);
    proc_names = calloc(size+1, sizeof(char*));
    for(int i = 0; i < size; i++) {
        proc_names[i] = strdup(c_cstr(procs[i]));
    }
    proc_names[size] = NULL;
    rl_attempted_completion_function = proc_name_completion_function;
}

static void save_repl_history() {
    char* path = get_history_filename();
    write_history(path);
}
static void repl() {
    atexit(save_repl_history);
    char* path = get_history_filename();
    char* line = NULL;
    scmval v;
    read_history(path);
    init_completion();
    while(true) {
        line = readline("> ");
        if(!line)
            break;
        add_history(line);
        with_error_handler(default_error_handler) {
            v = read_from_string(line);
            v = eval(v, scm_interaction_environment());
            if(!(is_undef(v) || is_void(v))) {
                write(scm_current_output_port(), v, scm_mode_write | scm_mode_pp_quote);
                scm_printf(scm_current_output_port(), "\n");
            }
        }
        free(line);
    }
    write_history(path);
}

// MISC
void dstats() {
    printf("\n** [heap: %zd] [free: %zd] [bytes-since-gc: %zd] [total: %zd]\n",
            GC_get_heap_size(),
            GC_get_free_bytes(),
            GC_get_bytes_since_gc(),
            GC_get_total_bytes());
}
