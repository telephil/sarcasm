#include "scm.h"

// globals
scmval scm_eof;

// constructors
scmval make_port(scmfix flags, void* data, char* name, scm_port_vtable_t* vtable) {
    scm_port_t* port = scm_new(scm_port_t);
    port->flags  = flags;
    port->data   = data;
    port->name   = name;
    port->open   = true;
    port->line   = 1;
    port->pos    = 0;
    port->vtable = vtable;
    return make_ptr(SCM_TYPE_PORT, port);
}

// ports creation
static scmval make_file_input_port(FILE*, char*);
static scmval make_file_input_port_from_filename(scmval);
static scmval make_string_input_port(scmval);
static scmval make_file_output_port(FILE*, char*);
static scmval make_file_output_port_from_filename(scmval);
static scmval make_string_output_port();

// port procedures
static scmval scm_port_p(scmval v) {
    return scm_bool(is_port(v));
}

static scmval scm_input_port_p(scmval v) {
    return scm_bool(is_input_port(v));
}

static scmval scm_output_port_p(scmval v) {
    return scm_bool(is_output_port(v));
}

static scmval scm_eof_p(scmval v) {
    return scm_bool(is_eof(v));
}

static scmval scm_port_open_p(scmval v) {
    check_arg("port-open?", port_c, v);
    return scm_bool(is_port_open(v));
}

static scmval scm_current_input_port() {
    return scm_context.current_input_port;
}

scmval scm_current_output_port() {
    return scm_context.current_output_port;
}

scmval scm_current_error_port() {
    return scm_context.current_error_port;
}

static scmval scm_open_input_string(scmval v) {
    check_arg("open-input-string", string_c, v);
    return  make_string_input_port(v);
}

static scmval scm_open_input_file(scmval v) {
    check_arg("open-input-file", string_c, v);
    return make_file_input_port_from_filename(v);
}

static scmval scm_close_input_port(scmval p) {
    check_arg("close-input-port", input_port_c, p);
    port_close(p);
    return scm_undef;
}

static scmval scm_open_output_file(scmval v) {
    check_arg("open-output-file", string_c, v);
    return make_file_output_port_from_filename(v);
}

static scmval scm_open_output_string() {
    return make_string_output_port();
}

static scmval scm_get_output_string(scmval p) {
    check_arg("get-output-string", output_string_port_c, p);
    scm_string_t* s = port_data(p);
    return make_ptr(SCM_TYPE_STRING, s);
}

static scmval scm_close_output_port(scmval p) {
    check_arg("close-output-port", output_port_c, p);
    port_close(p);
    return scm_undef;
}

static scmval scm_read_char(scmval p) {
    check_arg("read-char", input_port_c, p);
    return port_getc(p);
}

static scmval scm_peek_char(scmval p) {
    check_arg("peek-char", input_port_c, p);
    scmval c = port_getc(p);
    if(is_eof(c))
        return scm_eof;
    c = port_ungetc(p, c);
    return c;
}

static scmval scm_read_line(scmval p) {
    check_arg("read-line", input_port_c, p);
    int i = 0, len = 1024;
    scm_char_t* buf, c;
    scmval v;
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

static scmval scm_write(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("write", output_port_c, p);
    write(p, v, scm_mode_write);
    return scm_undef;
}

static scmval scm_write_char(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("write-char", output_port_c, p);
    port_putc(p, v);
    return scm_undef;
}

static scmval scm_display(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("display", output_port_c, p);
    write(p, v, scm_mode_display);
    return scm_undef;
}

static scmval scm_newline(scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("newline", output_port_c, p);
    port_putc(p, make_char('\n'));
    return scm_undef;
}

static scmval scm_flush_output_port(scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("flush-output-port", output_port_c, p);
    port_flush(p);
    return scm_undef;
}

void init_port(scm_ctx_t* ctx) {
    scm_eof   = make_val(SCM_TYPE_EOF);

    scm_context.current_output_port = make_file_output_port(stdout, "stdout");
    scm_context.current_error_port  = make_file_output_port(stderr, "stderr");
    scm_context.current_input_port  = make_file_input_port(stdin, "stdin");

    define("port?", scm_port_p, arity_exactly(1));
    define("input-port?", scm_input_port_p, arity_exactly(1));
    define("output-port?", scm_output_port_p, arity_exactly(1));
    define("eof-object?", scm_eof_p, arity_exactly(1));
    define("port-open?", scm_port_open_p, arity_exactly(1));
    define("current-input-port", scm_current_input_port, arity_exactly(0));
    define("current-output-port", scm_current_output_port, arity_exactly(0));
    define("current-error-port", scm_current_error_port, arity_exactly(0));
    define("open-input-string", scm_open_input_string, arity_exactly(1));
    define("open-input-file", scm_open_input_file, arity_exactly(1));
    define("close-input-port", scm_close_input_port, arity_exactly(1));
    define("open-output-string", scm_open_output_string, arity_exactly(0));
    define("get-output-string", scm_get_output_string, arity_exactly(1));
    define("open-output-file", scm_open_output_file, arity_exactly(1));
    define("close-output-port", scm_close_output_port, arity_exactly(1));
    define("read-char", scm_read_char, arity_exactly(1));
    define("peek-char", scm_peek_char, arity_exactly(1));
    define("read-line", scm_read_line, arity_exactly(1));
    define("write", scm_write, arity_or(1, 2));
    define("write-char", scm_write_char, arity_or(1, 2));
    define("display", scm_display, arity_or(1, 2));
    define("newline", scm_newline, arity_or(0, 1));
    define("flush-output-port", scm_flush_output_port, arity_or(0, 1));
}

////////////////////////////////////////////////////////////////////////////////
// U T I L I T I E S
////////////////////////////////////////////////////////////////////////////////
scmval open_input_string(const char* s) {
    return make_string_input_port(make_string(s));
}

scmval scm_to_string(scmval v) {
    scmval p = scm_open_output_string();
    scm_write(v, p);
    scm_close_output_port(p);
    return scm_get_output_string(p);
}

scm_char_t scm_getc(scmval p) {
    scmval c = port_getc(p);
    if(is_eof(c))
        return EOF;
    if(char_value(c) == '\n') port_line(p)++;
    port_pos(p)++;
    return char_value(c);
}

void scm_ungetc(scmval p, scm_char_t c) {
    port_ungetc(p, make_char(c));
    if(c == '\n') port_line(p)--;
    port_pos(p)--;
}

scm_char_t scm_peek(scmval p) {
    scm_char_t c = scm_getc(p);
    port_ungetc(p, make_char(c));
    return c;
}
void scm_putc(scmval p, scm_char_t c) { 
    port_putc(p, make_char(c)); 
}

void scm_puts(scmval p, CORD c) {
    port_puts(p, make_string_from_cord(c)); 
}

void scm_printf(scmval p, CORD format, ...) {
    CORD r;
    va_list ap;
    va_start(ap, format);
    CORD_vsprintf(&r, format, ap);
    va_end(ap);
    scm_puts(p, r);
}


////////////////////////////////////////////////////////////////////////////////
// P O R T S  I M P L E M E N T A T I O N S
////////////////////////////////////////////////////////////////////////////////
// -- FILE
static void file_close(scmval op) {
    FILE* fp = port_data(op);
    set_port_open(op, false);
    fclose(fp);
}

static scmval file_getc(scmval p) {
    FILE* fp = port_data(p);
    if(feof(fp))
        return scm_eof;
    char c = getc(fp);
    if(c == EOF)
        return scm_eof;
    return make_char(c);
}

static scmval file_ungetc(scmval p, scmval c) {
    FILE* fp = port_data(p);
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

static void file_putc(scmval op, scmval v) {
    FILE* fp = port_data(op);
    fputc(char_value(v), fp);
}

static void file_puts(scmval op, scmval v) {
    FILE* fp = port_data(op);
    CORD_fprintf(fp, "%r", string_value(v));
}

static void file_flush(scmval op) {
    FILE* fp = port_data(op);
    fflush(fp);
}

static scmval make_file_input_port(FILE* fp, char* name) {
    static scm_port_vtable_t vtable = { file_close, file_getc, file_ungetc, file_char_ready, NULL, NULL, NULL };
    scmval p = make_port(scm_port_input | scm_port_file, fp, name, &vtable);
    return p;
}

static scmval make_file_input_port_from_filename(scmval f) {
    FILE* fp = fopen(string_value(f), "r");
    // XXX
    return make_file_input_port(fp, string_to_cstr(f));
}

static scmval make_file_output_port(FILE* fp, char* name) {
    static scm_port_vtable_t vtable = { file_close, NULL, NULL, NULL, file_putc, file_puts, file_flush };
    return make_port(scm_port_output | scm_port_file, fp, name, &vtable);
}

static scmval make_file_output_port_from_filename(scmval f) {
    FILE* fp = fopen(string_value(f), "w");
    return make_file_output_port(fp, string_to_cstr(f));
}

////////////////////////////////////////////////////////////////////////////////
// -- STRING
static void string_close(scmval p) {
    if(is_input_port(p))
        scm_delete(port_data(p));
    set_port_open(p, false);
}

static scmval string_getc(scmval p) {
    scm_input_string_t* in = port_data(p);
    if(in->idx >= in->len)
        return scm_eof;
    char c = in->buf[in->idx++];
    return make_char(c);
}

static scmval string_ungetc(scmval p, scmval c) {
    scm_input_string_t* in = port_data(p);
    if(in->idx <= 0)
        return scm_eof;
    in->idx--;
    return c;
}

static scmval string_char_ready(scmval p) {
    scm_input_string_t* in = port_data(p);
    return scm_bool(in->idx >= in->len);
}

static void string_putc(scmval p, scmval v) {
    scm_string_t* s = port_data(p);
    CORD c = CORD_chars(char_value(v), 1);
    s->value = CORD_cat(s->value, c);
}

static void string_puts(scmval p, scmval v) {
    scm_string_t* s = port_data(p);
    s->value = CORD_cat(s->value, string_value(v));
}

static void string_flush(scmval p) {
}

static scmval make_string_input_port(scmval s) {
    static scm_port_vtable_t vtable = { string_close, string_getc, string_ungetc, string_char_ready, NULL, NULL, NULL };
    scm_input_string_t* in = scm_new(scm_input_string_t);
    in->buf = CORD_to_const_char_star(string_value(s));
    in->idx = 0;
    in->len = strlen(in->buf);
    return make_port(scm_port_input | scm_port_string, in, "string", &vtable);
}

static scmval make_string_output_port() {
    static scm_port_vtable_t vtable = { string_close, NULL, NULL, NULL, string_putc, string_puts, string_flush };
    scm_string_t* s = scm_new(scm_string_t);
    s->value = NULL;
    return make_port(scm_port_output | scm_port_string, s, "string", &vtable);
}

