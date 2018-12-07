typedef struct context scm_ctx_t;
typedef struct stack_frame stack_frame_t;

struct stack_frame {
    scm_fixnum_t argc;
    scmval *argv;
    scmval err;
    stack_frame_t* next;
};

struct context {
    scm_dict_t* symbols;
    scm_dict_t* globals;
    scmval toplevel;
    
    scmval current_output_port;
    scmval current_error_port;

    stack_frame_t* stack;
    jmp_buf err_buf;
};

scm_ctx_t* init_context();
// Call stack
void push_frame(scm_ctx_t*, size_t);
void pop_frame(scm_ctx_t*);

// Arguments
static inline void arg_set(scm_ctx_t* ctx, int index, scmval val) { ctx->stack->argv[index] = val; }
static inline scmval arg_ref(scm_ctx_t* ctx, int index) { return ctx->stack->argv[index]; }
static inline scmval arg_ref_opt(scm_ctx_t* ctx, int index, scmval opt) {
    if(ctx->stack->argc <= index)
        return opt;
    return ctx->stack->argv[index];
}
static inline void arg_ref_list(scm_ctx_t* ctx, int* argc, scmval** argv) {
    *argc = ctx->stack->argc;
    *argv = ctx->stack->argv;
}

static inline void set_error(scm_ctx_t* ctx, scmval e) { ctx->stack->err = e; }
