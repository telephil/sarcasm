typedef struct context scm_ctx_t;

struct context {
    scm_dict_t* symbols;

    scmval current_output_port;
    scmval current_error_port;
    scmval current_input_port;

    scmval err;
    jmp_buf err_buf;
};

// Global Context
extern scm_ctx_t scm_context;

void init_context();

static inline void set_error(scmval e) { scm_context.err = e; }
