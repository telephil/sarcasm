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
        repl();
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
// R E P L
////////////////////////////////////////////////////////////////////////////////
static void repl() {
    scmval env = scm_interaction_environment();
    import(lib("sarcasm", "repl"), env);
    with_error_handler(default_error_handler) {
        eval(list1(intern("repl")), env);
    }
}

////////////////////////////////////////////////////////////////////////////////
// E R R O R  H A N D L E R S
////////////////////////////////////////////////////////////////////////////////
static void default_error_handler(scmval err) {
    write(scm_current_error_port(), err, scm_mode_display);
    write(scm_current_error_port(), s_char('\n'), scm_mode_display);
}

