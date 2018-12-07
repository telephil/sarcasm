#include <stdio.h>

#include "scm.h"

// globals
scmval scm_eof;

// constructors
scmval make_output_port(enum port_type type, void* p, outp_vtable* vtable) {
    scm_output_port_t* outp = GC_MALLOC(sizeof(scm_output_port_t));
    outp->type = type;
    outp->port = p;
    outp->open = false;
    outp->vtable = vtable;
    return make_ptr(SCM_TYPE_OUTPUT_PORT, outp);
}
// output ports
static scmval make_file_output_port(FILE*);

// VTables ops
static void file_putc(scmval, scmval);
static void file_puts(scmval, scmval);
static void file_flush(scmval);
static void file_close(scmval);


// port procedures
static scmval scm_port_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_port(v))
        return scm_true;
    return scm_false;
}

static scmval scm_input_port_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_input_port(v))
        return scm_true;
    return scm_false;
}

static scmval scm_output_port_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_output_port(v))
        return scm_true;
    return scm_false;
}

static scmval scm_eof_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_eof(v))
        return scm_true;
    return scm_false;
}

static scmval scm_write(scm_ctx_t* ctx) {
    scmval v, p, s;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    s = to_str(v, true);
    get_output_port(p)->vtable->puts(p, s);
    return scm_undef;
}

static scmval scm_write_char(scm_ctx_t* ctx) {
    scmval v, p;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    get_output_port(p)->vtable->putc(p, v);
    return scm_undef;
}

static scmval scm_display(scm_ctx_t* ctx) {
    scmval v, p, s;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    s = to_str(v, false);
    get_output_port(p)->vtable->puts(p, s);
    return scm_undef;
}

static scmval scm_newline(scm_ctx_t* ctx) {
    scmval p;
    p = arg_ref_opt(ctx, 0, ctx->current_output_port);
    get_output_port(p)->vtable->putc(p, make_char('\n'));
    return scm_undef;
}

void init_port(scm_ctx_t* ctx) {
    scm_eof   = make_val(SCM_TYPE_EOF);

    ctx->current_output_port = make_file_output_port(stdout);
    ctx->current_error_port  = make_file_output_port(stderr);

    define(ctx, "port?", scm_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "input-port?", scm_input_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "output-port?", scm_output_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "eof-object?", scm_eof_p, arity_exactly(1), 1, any_c);
    define(ctx, "write", scm_write, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "write-char", scm_write_char, arity_or(1, 2), 2, char_c, output_port_c);
    define(ctx, "display", scm_display, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "newline", scm_newline, arity_or(0, 1), 1, output_port_c);
}

// Implementations
static scmval make_file_output_port(FILE* fp) {
    static outp_vtable vtable = { file_putc, file_puts, file_flush, file_close };
    scmval v;
    v = make_output_port(FILE_PORT, fp, &vtable);
    return v;
}

static void file_putc(scmval op, scmval v) {
    FILE* fp = get_output_port(op)->port;
    fputc(get_char(v), fp);
}

static void file_puts(scmval op, scmval v) {
    FILE* fp = get_output_port(op)->port;
    CORD_fprintf(fp, "%r", get_string(v)->value);
}

static void file_flush(scmval op) {
    FILE* fp = get_output_port(op)->port;
    fflush(fp);
}

static void file_close(scmval op) {
    FILE* fp = get_output_port(op)->port;
    fclose(fp);
}

