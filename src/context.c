#include <gc/gc.h>

#include "scm.h"

scm_ctx_t* init_context() {
    scm_ctx_t* ctx = scm_new(scm_ctx_t);
    ctx->symbols  = make_dict();
    ctx->globals  = make_dict();
    ctx->toplevel = make_env();
    ctx->stack = NULL;
    return ctx;
}

void push_frame(scm_ctx_t* ctx, size_t n) {
    stack_frame_t* stack = ctx->stack;
    stack_frame_t* frame = scm_new(stack_frame_t);
    frame->argc = n;
    frame->argv = NULL;
    if(n > 0) {
        frame->argv = scm_new_array(n, scmval);
    }
    frame->next = stack;
    ctx->stack = frame;
}

void pop_frame(scm_ctx_t* ctx) {
    stack_frame_t* top = ctx->stack;
    if(top == NULL) {
        ctx->stack = NULL;
    } else {
        ctx->stack = top->next;
    }
    scm_delete(top);
}

