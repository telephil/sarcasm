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
    init_pair(ctx);
    init_vector(ctx);
    init_port(ctx);
    init_reader(ctx);
    init_eval(ctx);

    return ctx;
}

static char* get_history_filename() {
    static const char* filename = ".scm_history";
    char path[4096];
    snprintf(path, 4096, "%s/%s", getenv("HOME"), filename);
    return strdup(path);
}


static void repl(scm_ctx_t* ctx) {
    char* path = get_history_filename();
    char* line = NULL;
    scmval in, v;
    read_history(path);
    while(true) {
        line = readline("> ");
        if(!line)
            break;
        if(*line == ',' && *(line+1) == 'q')
            break;
        try(ctx) {
            in = open_input_string(line);
            v  = read(ctx, in);
            v  = eval(ctx, v, ctx->toplevel);
            if(!is_undef(v)) {
                write(ctx->current_output_port, v, WRITE_MODE_DISPLAY);
                printf("\n");
            }
            add_history(line);
        } catch {
            write(ctx->current_error_port, ctx->err, WRITE_MODE_DISPLAY);
            fprintf(stderr, "\n");
        }
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

