#include <getopt.h>
#include "scm.h"

static void default_error_handler(scmval err);
static void repl();
static void run(const char*, scmval);
static void run_eval(const char*);

////////////////////////////////////////////////////////////////////////////////
// OPTIONS
////////////////////////////////////////////////////////////////////////////////
static struct option options[] = {
    { "help",   no_argument,       0, 'h' },
    { "eval",   required_argument, 0, 'e' },
    { "load",   required_argument, 0, 'l' },
    { "script", required_argument, 0, 's' },
    { 0, 0, 0, 0 }
};

enum { mode_eval, mode_load, mode_script, mode_repl };

static void usage() {
    printf("usage: scm [options]\n");
    printf("options:\n");
    printf("  -h, --help                    print help and exit\n");
    printf("  -e, --eval <form>             evaluate form and exit\n");
    printf("  -s, --script <filename>       execute file as a shell script\n");
    printf("  -l, --load <filename>         load file then run repl\n");
    exit(0);
}

////////////////////////////////////////////////////////////////////////////////
// M A I N
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {
    scm_boot(argc, argv);

    // parse command line
    int   mode  = mode_repl;
    char* arg   = NULL;
    while(true) {
        int option_index = 0;
        int c = getopt_long(argc, argv, "he:l:s:", options, &option_index);
        if(c == -1)
            break;
        switch(c) {
            case 'h':
                usage();
                break;
            case 'e':
                mode = mode_eval;
                arg  = optarg;
                break;
            case 'l':
                mode = mode_load;
                arg  = optarg;
                break;
            case 's':
                mode = mode_script;
                arg  = optarg;
                break;
            case '?':
                usage();
                break;
            default:
                abort();
                break;
        }
    }
    if(optind < argc) {
        printf("too many arguments: ");
        while(optind < argc) {
            printf("%s ", argv[optind++]);
        }
        printf("\n");
        return EXIT_FAILURE;
    }

    // run sarcasm
    scmval env;
    switch(mode) {
        case mode_eval:
            run_eval(arg);
            break;
        case mode_script: // load file and exit
            env = make_env(scm_undef);
            run(arg, env);
            break;
        case mode_load: // load file then run repl
            env = scm_interaction_environment();
            run(arg, env);
        case mode_repl:
            repl();
            break;
    }
    return EXIT_SUCCESS;
}

static void run(const char* filename, scmval env) {
    with_error_handler(default_error_handler) {
        load(filename, env);
    }
}

static void run_eval(const char* expr) {
    scmval e = scm_interaction_environment();
    scmval r = scm_undef;
    scmval p = open_input_string(expr);
    with_error_handler(default_error_handler) {
        while(true) {
            scmval v = scm_read(p);
            if(is_eof(v))
                break;
            r = eval(v, e);
        }
    }
    scm_close_input_port(p);
    scm_display(r, scm_current_output_port());
    scm_display(s_char('\n'), scm_current_output_port());
}

static void repl() {
    scmval env = scm_interaction_environment();
    import(lib("sarcasm", "repl"), env);
    with_error_handler(default_error_handler) {
        eval(list1(intern("repl")), env);
    }
}

static void default_error_handler(scmval err) {
    scm_display(err, scm_current_error_port());
    scm_display(s_char('\n'), scm_current_error_port());
}

