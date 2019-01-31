#include <readline/readline.h>
#include <readline/history.h>
#include <scm.h>

////////////////////////////////////////////////////////////////////////////////
// BASE READLINE FUNCTIONS
////////////////////////////////////////////////////////////////////////////////
static scmval scm_readline(scmval prompt) {
    check_arg("readline", string_c, prompt);
    char* line = readline(c_str(prompt));
    if(line == NULL)
        return scm_false;
    scmval s = s_str(line);
    free(line);
    return s;
}

static scmval scm_read_history(scmval filename) {
    check_arg("read-history", string_c, filename);
    read_history(c_str(filename));
    return scm_void;
}

static scmval scm_write_history(scmval filename) {
    check_arg("write-history", string_c, filename);
    write_history(c_str(filename));
    return scm_void;
}

static scmval scm_add_history(scmval history) {
    check_arg("add-history", string_c, history);
    add_history(c_str(history));
    return scm_void;
}

////////////////////////////////////////////////////////////////////////////////
// COMPLETION
////////////////////////////////////////////////////////////////////////////////
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
        completions[i++] = strdup(c_str(s));
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

////////////////////////////////////////////////////////////////////////////////
// MODULE INITIALIZATION
////////////////////////////////////////////////////////////////////////////////
void init_module(scmval env) {
    define(env, "readline",                 scm_readline,                   arity_exactly(1));
    define(env, "read-history",             scm_read_history,               arity_exactly(1));
    define(env, "write-history",            scm_write_history,              arity_exactly(1));
    define(env, "add-history",              scm_add_history,                arity_exactly(1));
    define(env, "set-completion-function!", scm_set_completion_function,    arity_exactly(1));
}

