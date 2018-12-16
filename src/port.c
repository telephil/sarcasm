#include "scm.h"

// globals
scmval scm_eof;

// constructors
scmval make_input_port(enum port_type type, void* p, ip_vtable* vtable) {
    scm_input_port_t* ip = scm_new(scm_input_port_t);
    ip->type = type;
    ip->port = p;
    ip->open = true;
    ip->line = 1;
    ip->pos  = 0;
    ip->vtable = vtable;
    return make_ptr(SCM_TYPE_INPUT_PORT, ip);
}

scmval make_output_port(enum port_type type, void* p, outp_vtable* vtable) {
    scm_output_port_t* outp = scm_new(scm_output_port_t);
    outp->type = type;
    outp->port = p;
    outp->open = true;
    outp->vtable = vtable;
    return make_ptr(SCM_TYPE_OUTPUT_PORT, outp);
}
// ports creation
static scmval make_file_input_port(FILE*);
static scmval make_file_input_port_from_filename(scmval);
static scmval make_string_input_port(scmval);
static scmval make_file_output_port(FILE*);
static scmval make_file_output_port_from_filename(scmval);
static scmval make_string_output_port();

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

static scmval scm_current_input_port(scm_ctx_t* ctx) {
    return ctx->current_input_port;
}

static scmval scm_current_output_port(scm_ctx_t* ctx) {
    return ctx->current_output_port;
}

static scmval scm_current_error_port(scm_ctx_t* ctx) {
    return ctx->current_error_port;
}

static scmval scm_open_input_string(scm_ctx_t* ctx) {
    scmval p, v;
    v = arg_ref(ctx, 0);
    p = make_string_input_port(v);
    return p;
}

static scmval scm_open_input_file(scm_ctx_t* ctx) {
    scmval p, v;
    v = arg_ref(ctx, 0);
    p = make_file_input_port_from_filename(v);
    return p;
}

static scmval scm_close_input_port(scm_ctx_t* ctx) {
    scmval p;
    p = arg_ref(ctx, 0);
    input_port_close(p);
    return scm_undef;
}

static scmval scm_open_output_file(scm_ctx_t* ctx) {
    scmval p, v;
    v = arg_ref(ctx, 0);
    p = make_file_output_port_from_filename(v);
    return p;
}

static scmval scm_open_output_string(scm_ctx_t* ctx) {
    scmval p;
    p = make_string_output_port();
    return p;
}

static scmval scm_get_output_string(scm_ctx_t* ctx) {
    scmval v;
    scm_string_t* s;
    v = arg_ref(ctx, 0);
    s = output_port_port(v);
    return make_ptr(SCM_TYPE_STRING, s);
}

static scmval scm_close_output_port(scm_ctx_t* ctx) {
    scmval p;
    p = arg_ref(ctx, 0);
    output_port_close(p);
    return scm_undef;
}

static scmval scm_read_char(scm_ctx_t* ctx) {
    scmval c, p;
    p = arg_ref(ctx, 0);
    c = input_port_getc(p);
    return c;
}

static scmval scm_peek_char(scm_ctx_t* ctx) {
    scmval c, p;
    p = arg_ref(ctx, 0);
    c = input_port_getc(p);
    if(is_eof(c))
        return scm_eof;
    c = input_port_ungetc(p, c);
    return c;
}

static scmval scm_read_line(scm_ctx_t* ctx) {
    int i = 0, len = 1024;
    scm_char_t* buf, c;
    scmval v, p;
    p = arg_ref(ctx, 0);
    buf = scm_new_array(len, scm_char_t);
    while(true) {
        c = scm_getc(p);
        if(c == EOF)
            break;
        if(c == '\n')
            break;
        buf[i++] = c;
    }
    if(i == 0 && c == EOF)
        return scm_eof;
    buf[i] = '\0';
    v = make_string(buf);
    return v;
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
    ctx->current_input_port  = make_file_input_port(stdin);

    define(ctx, "port?", scm_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "input-port?", scm_input_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "output-port?", scm_output_port_p, arity_exactly(1), 1, any_c);
    define(ctx, "eof-object?", scm_eof_p, arity_exactly(1), 1, any_c);
    define(ctx, "port-open?", scm_port_open_p, arity_exactly(1), 1, port_c);
    define(ctx, "current-input-port", scm_current_input_port, arity_exactly(0), 0);
    define(ctx, "current-output-port", scm_current_output_port, arity_exactly(0), 0);
    define(ctx, "current-error-port", scm_current_error_port, arity_exactly(0), 0);
    define(ctx, "open-input-string", scm_open_input_string, arity_exactly(1), 1, string_c);
    define(ctx, "open-input-file", scm_open_input_file, arity_exactly(1), 1, string_c);
    define(ctx, "close-input-port", scm_close_input_port, arity_exactly(1), 1, input_port_c);
    define(ctx, "open-output-string", scm_open_output_string, arity_exactly(1), 0);
    define(ctx, "get-output-string", scm_get_output_string, arity_exactly(1), 1, output_port_c);
    define(ctx, "open-output-file", scm_open_output_file, arity_exactly(1), 1, string_c);
    define(ctx, "close-output-port", scm_close_output_port, arity_exactly(1), 1, output_port_c);
    define(ctx, "read-char", scm_read_char, arity_exactly(1), 1, input_port_c);
    define(ctx, "peek-char", scm_peek_char, arity_exactly(1), 1, input_port_c);
    define(ctx, "read-line", scm_read_line, arity_exactly(1), 1, input_port_c);
    define(ctx, "write", scm_write, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "write-char", scm_write_char, arity_or(1, 2), 2, char_c, output_port_c);
    define(ctx, "display", scm_display, arity_or(1, 2), 2, any_c, output_port_c);
    define(ctx, "newline", scm_newline, arity_or(0, 1), 1, output_port_c);
    define(ctx, "flush-output-port", scm_flush_output_port, arity_or(0, 1), 1, output_port_c);
}

// INPUT PORTS implementations
// -- FILE
static scmval file_getc(scmval p) {
    FILE* fp = input_port_port(p);
    if(feof(fp))
        return scm_eof;
    char c = getc(fp);
    if(c == EOF)
        return scm_eof;
    return make_char(c);
}

static scmval file_ungetc(scmval p, scmval c) {
    FILE* fp = input_port_port(p);
    if(feof(fp))
        return scm_eof;
    char ch = ungetc(char_value(c), fp);
    return make_char(ch);
}

static scmval file_char_ready(scmval p) {
    if(is_port_open(p))
        return scm_true; // XXX is this enough ?
    return scm_false;
}

static void file_ip_close(scmval p) {
    FILE* fp = input_port_port(p);
    fclose(fp);
    set_port_open(p, false);
}

static scmval make_file_input_port(FILE* fp) {
    static struct ip_vtable vtable = { file_getc, file_ungetc, file_char_ready, file_ip_close };
    scmval p = make_input_port(FILE_PORT, fp, &vtable);
    return p;
}

static scmval make_file_input_port_from_filename(scmval f) {
    FILE* fp = fopen(string_value(f), "r");
    // XXX
    return make_file_input_port(fp);
}

// -- STRING
static scmval string_getc(scmval p) {
    scm_input_string_t* in = input_port_port(p);
    if(in->idx >= in->len)
        return scm_eof;
    char c = in->buf[in->idx++];
    return make_char(c);
}

static scmval string_ungetc(scmval p, scmval c) {
    scm_input_string_t* in = input_port_port(p);
    if(in->idx <= 0)
        return scm_eof;
    in->idx--;
    return c;
}

static scmval string_char_ready(scmval p) {
    scm_input_string_t* in = input_port_port(p);
    if(in->idx >= in->len)
        return scm_false;
    return scm_true;
}

static void string_ip_close(scmval p) {
    scm_delete(input_port_port(p));
    set_port_open(p, false);
}

static scmval make_string_input_port(scmval s) {
    static struct ip_vtable vtable = { string_getc, string_ungetc, string_char_ready, string_ip_close };
    scmval p;
    scm_input_string_t* in = scm_new(scm_input_string_t);
    in->buf = CORD_to_const_char_star(string_value(s));
    in->idx = 0;
    in->len = strlen(in->buf);
    p = make_input_port(STRING_PORT, in, &vtable);
    return p;
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

static scmval make_file_output_port_from_filename(scmval f) {
    FILE* fp = fopen(string_value(f), "w");
    return make_file_output_port(fp);
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

scmval open_input_string(const char* s) {
    return make_string_input_port(make_string(s));
}

