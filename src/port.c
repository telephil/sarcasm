#include <stdio.h>

#include "scm.h"

// globals
scmval scm_eof;

// constructors
scmval make_output_port(enum port_type type, void* p, outp_vtable* vtable) {
    scm_output_port_t* outp = scm_new(scm_output_port_t);
    outp->type = type;
    outp->port = p;
    outp->open = true;
    outp->vtable = vtable;
    return make_ptr(SCM_TYPE_OUTPUT_PORT, outp);
}
// output ports
static scmval make_file_output_port(FILE*);


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

static scmval scm_port_open_p(scm_ctx_t* ctx) {
    scmval v;
    v = arg_ref(ctx, 0);
    if(is_port_open(v))
        return scm_true;
    return scm_false;
}

static scmval scm_write(scm_ctx_t* ctx) {
    scmval v, p;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    write(p, v, WRITE_MODE_WRITE);
    return scm_undef;
}

static scmval scm_write_char(scm_ctx_t* ctx) {
    scmval v, p;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    output_port_putc(p, v);
    return scm_undef;
}

static scmval scm_display(scm_ctx_t* ctx) {
    scmval v, p; //, s;
    v = arg_ref(ctx, 0);
    p = arg_ref_opt(ctx, 1, ctx->current_output_port);
    write(p, v, WRITE_MODE_DISPLAY);
    return scm_undef;
}

static scmval scm_newline(scm_ctx_t* ctx) {
    scmval p;
    p = arg_ref_opt(ctx, 0, ctx->current_output_port);
    output_port_putc(p, make_char('\n'));
    return scm_undef;
}

static scmval scm_flush_output_port(scm_ctx_t* ctx) {
    scmval p;
    p = arg_ref_opt(ctx, 0, ctx->current_output_port);
    output_port_flush(p);
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
    define(ctx, "port-open?", scm_port_open_p, arity_exactly(1), 1, port_c);
    define(ctx, "write", scm_write, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "write-char", scm_write_char, arity_or(1, 2), 2, char_c, output_port_c);
    define(ctx, "display", scm_display, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "newline", scm_newline, arity_or(0, 1), 1, output_port_c);
    define(ctx, "flush-output-port", scm_flush_output_port, arity_or(0, 1), 1, output_port_c);
}

// OUTPUT PORTS implementations
// -- FILE
static void file_putc(scmval op, scmval v) {
    FILE* fp = output_port_port(op);
    fputc(char_value(v), fp);
}

static void file_puts(scmval op, scmval v) {
    FILE* fp = output_port_port(op);
    CORD_fprintf(fp, "%r", string_value(v));
}

static void file_flush(scmval op) {
    FILE* fp = output_port_port(op);
    fflush(fp);
}

static void file_close(scmval op) {
    FILE* fp = output_port_port(op);
    set_port_open(op, false);
    fclose(fp);
}

static scmval make_file_output_port(FILE* fp) {
    static outp_vtable vtable = { file_putc, file_puts, file_flush, file_close };
    scmval v;
    v = make_output_port(FILE_PORT, fp, &vtable);
    return v;
}

// -- STRING
static void string_putc(scmval p, scmval v) {
    scm_string_t* s = output_port_port(p);
    CORD c = CORD_chars(char_value(v), 1);
    s->value = CORD_cat(s->value, c);
}

static void string_puts(scmval p, scmval v) {
    scm_string_t* s = output_port_port(p);
    s->value = CORD_cat(s->value, string_value(v));
}

static void string_flush(scmval p) {
}

static void string_close(scmval p) {
    set_port_open(p, false);
}

static scmval make_string_output_port() {
    static outp_vtable vtable = { string_putc, string_puts, string_flush, string_close };
    scmval v;
    scm_string_t* s = scm_new(scm_string_t);
    s->value = NULL;
    v = make_output_port(STRING_PORT, s, &vtable);
    return v;
}

scmval open_output_string() {
    return make_string_output_port();
}

scmval get_output_string(scmval v) {
    scm_string_t* s = output_port_port(v);
    return make_ptr(SCM_TYPE_STRING, s);
}
