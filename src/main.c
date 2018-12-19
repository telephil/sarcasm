#include <readline/readline.h>
#include <readline/history.h>
#include "scm.h"

void dstats() {
    printf("\n** [heap: %zd] [free: %zd] [bytes-since-gc: %zd] [total: %zd]\n",
            GC_get_heap_size(),
            GC_get_free_bytes(),
            GC_get_bytes_since_gc(),
            GC_get_total_bytes());
}

static scm_ctx_t* scm_init() {
    scm_ctx_t* ctx;

    GC_INIT();

    ctx = init_context();
    init_errors(ctx);
    init_bool(ctx);
    init_char(ctx);
    init_symbol(ctx);
    init_pair(ctx);
    init_vector(ctx);
    init_port(ctx);
    init_reader(ctx);
    init_eval(ctx);

    return ctx;
}

//
// REPL
//
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

static void init_completion(scm_ctx_t* ctx) {
    scm_fixnum_t size;
    scmval* procs;
    dict_keys(ctx->globals, &size, &procs);
    proc_names = calloc(size+1, sizeof(char*));
    for(int i = 0; i < size; i++) {
        proc_names[i] = strdup(string_to_cstr(procs[i]));
    }
    proc_names[size] = NULL;
    rl_attempted_completion_function = proc_name_completion_function;
}

static void repl(scm_ctx_t* ctx) {
    char* path = get_history_filename();
    char* line = NULL;
    scmval in, v;
    read_history(path);
    init_completion(ctx);
    while(true) {
        line = readline("> ");
        if(!line)
            break;
        if(*line == ',' && *(line+1) == 'q')
            break;
        add_history(line);
        try(ctx) {
            in = open_input_string(line);
            v  = read(ctx, in);
            v  = eval(ctx, v, ctx->toplevel);
            if(!is_undef(v)) {
                write(ctx->current_output_port, v, WRITE_MODE_WRITE);
                printf("\n");
            }
        } catch {
            write(ctx->current_error_port, ctx->err, WRITE_MODE_WRITE);
            fprintf(stderr, "\n");
        }
        free(line);
    }
    write_history(path);
}

int main(int argc, char* argv[]) {
    scm_ctx_t* ctx;

    printf("scm v0.1\n");

    ctx = scm_init();
    repl(ctx);

    printf("Bye.\n");
    return 0;
}

