#include <sys/errno.h>
#include "scm.h"

static const int DEFAULT_BYTEVECTOR_PORT_SIZE = 1024;
// internal bytevector port data
typedef struct scm_bytes_buffer scm_bytes_buffer_t;
struct scm_bytes_buffer {
    byte* data;
    int   len;
    int   idx;
};

// globals
scmval scm_g_current_output_port;
scmval scm_g_current_error_port;
scmval scm_g_current_input_port;
scmval scm_eof;

// constructors
scmval make_port(short flags, void* data, char* name, scm_port_vtable_t* vtable) {
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

// error
static inline void file_error(const char* message, scmval filename, char* err) {
    error(file_error_type, message, c_cstr(filename), err);
}

// ports creation
static scmval make_file_input_port(FILE*, char*, short);
static scmval make_file_input_port_from_filename(scmval);
static scmval make_binary_file_input_port_from_filename(scmval);
static scmval scm_str_input_port(scmval);
static scmval make_file_output_port(FILE*, char*, short);
static scmval make_file_output_port_from_filename(scmval);
static scmval make_binary_file_output_port_from_filename(scmval);
static scmval scm_str_output_port();
static scmval make_bytevector_input_port(scmval);
static scmval make_bytevector_output_port();

// port procedures
static scmval scm_port_p(scmval v) {
    return s_bool(is_port(v));
}

static scmval scm_input_port_p(scmval v) {
    return s_bool(is_input_port(v));
}

static scmval scm_output_port_p(scmval v) {
    return s_bool(is_output_port(v));
}

static scmval scm_textual_port_p(scmval v) {
    return s_bool(is_textual_port(v));
}

static scmval scm_binary_port_p(scmval v) {
    return s_bool(is_binary_port(v));
}

static scmval scm_eof_p(scmval v) {
    return s_bool(is_eof(v));
}

static scmval scm_port_open_p(scmval v) {
    check_arg("port-open?", port_c, v);
    return s_bool(is_port_open(v));
}

static scmval scm_eof_object() {
    return scm_eof;
}

static scmval scm_current_input_port() {
    return scm_g_current_input_port;
}

scmval scm_current_output_port() {
    return scm_g_current_output_port;
}

scmval scm_current_error_port() {
    return scm_g_current_error_port;
}

static scmval scm_set_current_output_port(scmval port) {
    check_arg("%set-current-output-port", output_port_c, port);
    scm_g_current_output_port = port;
    return scm_void;
}

static scmval scm_set_current_input_port(scmval port) {
    check_arg("%set-current-input-port", input_port_c, port);
    scm_g_current_input_port = port;
    return scm_void;
}

static scmval scm_open_input_string(scmval v) {
    check_arg("open-input-string", string_c, v);
    return  scm_str_input_port(v);
}

scmval scm_open_input_file(scmval v) {
    check_arg("open-input-file", string_c, v);
    return make_file_input_port_from_filename(v);
}

static scmval scm_open_binary_input_file(scmval v) {
    check_arg("open-binary-input-file", string_c, v);
    return make_binary_file_input_port_from_filename(v);
}

scmval scm_close_input_port(scmval p) {
    check_arg("close-input-port", input_port_c, p);
    port_close(p);
    return scm_void;
}

static scmval scm_open_output_file(scmval v) {
    check_arg("open-output-file", string_c, v);
    return make_file_output_port_from_filename(v);
}

static scmval scm_open_binary_output_file(scmval v) {
    check_arg("open-binary-output-file", string_c, v);
    return make_binary_file_output_port_from_filename(v);
}

scmval scm_open_output_string() {
    return scm_str_output_port();
}

scmval scm_get_output_string(scmval p) {
    check_arg("get-output-string", output_string_port_c, p);
    scm_string_t* s = port_data(p);
    return make_ptr(SCM_TYPE_STRING, s);
}

static scmval scm_open_input_bytevector(scmval b) {
    check_arg("open-input-bytevector", bytevector_c, b);
    return make_bytevector_input_port(b);
}

static scmval scm_open_output_bytevector() {
    return make_bytevector_output_port();
}

static scmval scm_get_output_bytevector(scmval p) {
    check_arg("get-output-bytevector", output_bytevector_port_c, p);
    scm_bytes_buffer_t* out = port_data(p);
    int len = out->idx + 1;
    byte* data = scm_new_atomic(len, byte);
    memcpy(data, out->data, len * sizeof(byte));
    return make_bytevector_from_data(len, data);
}

static scmval scm_close_output_port(scmval p) {
    check_arg("close-output-port", output_port_c, p);
    port_close(p);
    return scm_void;
}

static scmval scm_char_ready_p(scmval p) {
    opt_arg(p, scm_current_input_port());
    check_arg("char-ready?", input_port_c, p);
    return port_ready(p);
}

static scmval scm_read(scmval p) {
    opt_arg(p, scm_current_input_port());
    check_arg("read", textual_input_port_c, p);
    return read(p);
}

static scmval scm_read_char(scmval p) {
    opt_arg(p, scm_current_input_port());
    check_arg("read-char", textual_input_port_c, p);
    return port_getc(p);
}

static scmval scm_peek_char(scmval p) {
    opt_arg(p, scm_current_input_port());
    check_arg("peek-char", textual_input_port_c, p);
    scmval c = port_getc(p);
    if(is_eof(c))
        return scm_eof;
    c = port_ungetc(p, c);
    return c;
}

static scmval scm_read_line(scmval p) {
    opt_arg(p, scm_current_input_port());
    check_arg("read-line", textual_input_port_c, p);
    int i = 0, len = 1024;
    char* buf, c;
    scmval v;
    buf = scm_new_atomic(len, char);
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
    v = s_str(buf);
    return v;
}

static scmval scm_read_string(scmval k, scmval port) {
    opt_arg(port, scm_current_input_port());
    check_arg("read-string", fixnum_c, k);
    check_arg("read-string", textual_input_port_c, port);
    char* buf = scm_new_atomic(c_fix(k) + 1, char);
    int i = 0;
    for(i = 0; i < c_fix(k); i++) {
        char c = scm_getc(port);
        if(c == EOF)
            break;
        buf[i] = c;
    }
    buf[i] = '\0';
    return s_str(buf);
}

static scmval scm_read_u8(scmval port) {
    opt_arg(port, scm_current_input_port());
    check_arg("read-u8", binary_input_port_c, port);
    return port_getb(port);
}

static scmval scm_peek_u8(scmval port) {
    opt_arg(port, scm_current_input_port());
    check_arg("peek-u8", binary_input_port_c, port);
    return port_peekb(port);
}

static scmval scm_u8_ready_p(scmval port) {
    opt_arg(port, scm_current_input_port());
    check_arg("u8-ready?", binary_input_port_c, port);
    return port_ready(port);
}

static scmval scm_read_bytevector(scmval k, scmval port) {
    opt_arg(port, scm_current_input_port());
    check_arg("read-bytevector", fixnum_c, k);
    check_arg("read-bytevector", binary_input_port_c, port);
    byte* data = scm_new_atomic(c_fix(k), byte);
    int i = 0;
    for(i = 0; i < c_fix(k); i++) {
        scmval b = scm_read_u8(port);
        if(is_eof(b))
            break;
        data[i] = c_fix(b);
    }
    return make_bytevector_from_data(i, data);
}

static scmval scm_read_bytevector_mut(scmval bv, scmval port, scmval start, scmval end) {
    opt_arg(port,   scm_current_output_port());
    opt_arg(start,  scm_0);
    opt_arg(end,    s_fix(bytevector_size(bv)-1));
    check_arg("read-bytevector!", bytevector_c, bv);
    check_arg("read-bytevector!", binary_input_port_c, port);
    check_arg("read-bytevector!", fixnum_c, start);
    check_arg("read-bytevector!", fixnum_c, end);
    check_range("read-bytevector!", c_fix(start), 0, bytevector_size(bv));
    check_range("read-bytevector!", c_fix(end), c_fix(start), bytevector_size(bv));
    int count = 0;
    for(int i = c_fix(start); i <= c_fix(end); i++) {
        scmval b = scm_read_u8(port);
        if(is_eof(b)) {
            if(count == 0) return scm_eof;
            break;
        }
        bytevector_set(bv, i, b);
        ++count;
    }
    return s_fix(count);
}

static scmval scm_write(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("write", textual_output_port_c, p);
    write(p, v, scm_mode_write | scm_mode_pp_quote);
    return scm_void;
}

static scmval scm_write_char(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("write-char", textual_output_port_c, p);
    port_putc(p, v);
    return scm_void;
}

static scmval scm_write_string(scmval str, scmval port, scmval start, scmval end) {
    opt_arg(port,   scm_current_output_port());
    opt_arg(start,  scm_0);
    opt_arg(end,    s_fix(string_length(str)-1));
    check_arg("write-string", string_c, str);
    check_arg("write-string", textual_output_port_c, port);
    check_arg("write-string", fixnum_c, start);
    check_arg("write-string", fixnum_c, end);
    check_range("write-string", c_fix(start), 0, string_length(str));
    check_range("write-string", c_fix(end), c_fix(start), string_length(str));
    char *cs = c_cstr(str);
    for(int i = c_fix(start); i <= c_fix(end); i++) {
        scm_putc(port, cs[i]);
    }
    return scm_void;
}

static scmval scm_write_u8(scmval b, scmval port) {
    opt_arg(port, scm_current_output_port());
    check_arg("write-u8", byte_c, b);
    check_arg("write-u8", binary_output_port_c, port);
    port_putb(port, b);
    return scm_void;
}

static scmval scm_write_bytevector(scmval bv, scmval port, scmval start, scmval end) {
    opt_arg(port,   scm_current_output_port());
    opt_arg(start,  scm_0);
    opt_arg(end,    s_fix(bytevector_size(bv)-1));
    check_arg("write-bytevector", bytevector_c, bv);
    check_arg("write-bytevector", binary_output_port_c, port);
    check_arg("write-bytevector", fixnum_c, start);
    check_arg("write-bytevector", fixnum_c, end);
    check_range("write-bytevector", c_fix(start), 0, bytevector_size(bv));
    check_range("write-bytevector", c_fix(end), c_fix(start), bytevector_size(bv));
    for(int i = c_fix(start); i <= c_fix(end); i++) {
        port_putb(port, bytevector_ref(bv, i));
    }
    return scm_void;
}

static scmval scm_display(scmval v, scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("display", textual_output_port_c, p);
    write(p, v, scm_mode_display | scm_mode_pp_quote);
    return scm_void;
}

static scmval scm_newline(scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("newline", textual_output_port_c, p);
    port_putc(p, s_char('\n'));
    return scm_void;
}

static scmval scm_flush_output_port(scmval p) {
    opt_arg(p, scm_current_output_port());
    check_arg("flush-output-port", output_port_c, p);
    port_flush(p);
    return scm_void;
}

static scmval scm_close_port(scmval p) {
    check_arg("close-port", port_c, p);
    port_close(p);
    return scm_void;
}

void init_port(scmval env) {
    // XXX convert to parameters
    scm_g_current_output_port = make_file_output_port(stdout, "stdout", scm_port_text);
    scm_g_current_error_port  = make_file_output_port(stderr, "stderr", scm_port_text);
    scm_g_current_input_port  = make_file_input_port(stdin, "stdin", scm_port_text);

    define(env, "port?",                    scm_port_p,                     arity_exactly(1));
    define(env, "input-port?",              scm_input_port_p,               arity_exactly(1));
    define(env, "output-port?",             scm_output_port_p,              arity_exactly(1));
    define(env, "eof-object?",              scm_eof_p,                      arity_exactly(1));
    define(env, "eof-object",               scm_eof_object,                 arity_exactly(0));
    define(env, "port-open?",               scm_port_open_p,                arity_exactly(1));
    define(env, "binary-port?",             scm_binary_port_p,              arity_exactly(1));
    define(env, "textual-port?",            scm_textual_port_p,             arity_exactly(1));
    define(env, "current-input-port",       scm_current_input_port,         arity_exactly(0));
    define(env, "current-output-port",      scm_current_output_port,        arity_exactly(0));
    define(env, "current-error-port",       scm_current_error_port,         arity_exactly(0));
    define(env, "%set-current-output-port", scm_set_current_output_port,    arity_exactly(1));
    define(env, "%set-current-input-port",  scm_set_current_input_port,     arity_exactly(1));
    define(env, "open-input-string",        scm_open_input_string,          arity_exactly(1));
    define(env, "open-input-file",          scm_open_input_file,            arity_exactly(1));
    define(env, "open-binary-input-file",   scm_open_binary_input_file,     arity_exactly(1));
    define(env, "close-input-port",         scm_close_input_port,           arity_exactly(1));
    define(env, "open-output-string",       scm_open_output_string,         arity_exactly(0));
    define(env, "get-output-string",        scm_get_output_string,          arity_exactly(1));
    define(env, "open-input-bytevector",    scm_open_input_bytevector,      arity_exactly(1));
    define(env, "open-output-bytevector",   scm_open_output_bytevector,     arity_exactly(0));
    define(env, "get-output-bytevector",    scm_get_output_bytevector,      arity_exactly(1));
    define(env, "open-output-file",         scm_open_output_file,           arity_exactly(1));
    define(env, "open-binary-output-file",  scm_open_binary_output_file,    arity_exactly(1));
    define(env, "close-output-port",        scm_close_output_port,          arity_exactly(1));
    define(env, "close-port",               scm_close_port,                 arity_exactly(1));
    define(env, "char-ready?",              scm_char_ready_p,               arity_or(0, 1));
    define(env, "read",                     scm_read,                       arity_or(0, 1));
    define(env, "read-char",                scm_read_char,                  arity_or(0, 1));
    define(env, "peek-char",                scm_peek_char,                  arity_or(0, 1));
    define(env, "read-line",                scm_read_line,                  arity_or(0, 1));
    define(env, "read-string",              scm_read_string,                arity_or(1, 2));
    define(env, "read-u8",                  scm_read_u8,                    arity_or(0, 1));
    define(env, "peek-u8",                  scm_peek_u8,                    arity_or(0, 1));
    define(env, "u8-ready?",                scm_u8_ready_p,                 arity_or(0, 1));
    define(env, "read-bytevector",          scm_read_bytevector,            arity_or(1, 2));
    define(env, "read-bytevector!",         scm_read_bytevector_mut,        arity_between(1, 4));
    define(env, "write",                    scm_write,                      arity_or(1, 2));
    define(env, "write-char",               scm_write_char,                 arity_or(1, 2));
    define(env, "write-string",             scm_write_string,               arity_between(1, 4));
    define(env, "write-u8",                 scm_write_u8,                   arity_or(1, 2));
    define(env, "write-bytevector",         scm_write_bytevector,           arity_between(1, 4));
    define(env, "display",                  scm_display,                    arity_or(1, 2));
    define(env, "newline",                  scm_newline,                    arity_or(0, 1));
    define(env, "flush-output-port",        scm_flush_output_port,          arity_or(0, 1));
}

////////////////////////////////////////////////////////////////////////////////
// U T I L I T I E S
////////////////////////////////////////////////////////////////////////////////
scmval open_input_string(const char* s) {
    return scm_str_input_port(s_str(s));
}

scmval scm_to_string(scmval v) {
    scmval p = scm_open_output_string();
    scm_write(v, p);
    scm_close_output_port(p);
    return scm_get_output_string(p);
}

char scm_getc(scmval p) {
    scmval c = port_getc(p);
    if(is_eof(c))
        return EOF;
    if(c_char(c) == '\n') port_line(p)++;
    port_pos(p)++;
    return c_char(c);
}

void scm_ungetc(scmval p, char c) {
    port_ungetc(p, s_char(c));
    if(c == '\n') port_line(p)--;
    port_pos(p)--;
}

char scm_peek(scmval p) {
    char c = scm_getc(p);
    port_ungetc(p, s_char(c));
    return c;
}
void scm_putc(scmval p, char c) { 
    port_putc(p, s_char(c)); 
}

void scm_puts(scmval p, CORD c) {
    port_puts(p, scm_str_from_cord(c)); 
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
    return s_char(c);
}

static scmval file_ungetc(scmval p, scmval c) {
    FILE* fp = port_data(p);
    if(feof(fp))
        return scm_eof;
    char ch = ungetc(c_char(c), fp);
    return s_char(ch);
}

static scmval file_getb(scmval p) {
    FILE* fp = port_data(p);
    if(feof(fp))
        return scm_eof;
    byte b[1];
    size_t count = fread(b, sizeof(byte), 1, fp);
    if(count != 1) {
        if(feof(fp))
            return scm_eof;
        if(ferror(fp))
            file_error("unable to read u8 from port %s (%s)", s_str(port_name(p)), strerror(errno));
    }
    return s_fix(b[0]);
}

static scmval file_peekb(scmval p) {
    FILE* fp = port_data(p);
    if(feof(fp))
        return scm_eof;
    fpos_t pos;
    int ret = fgetpos(fp, &pos);
    if(ret != 0)
        file_error("unable to peek at port %s (%s)", s_str(port_name(p)), strerror(errno));
    scmval b = file_getb(p);
    ret = fsetpos(fp, &pos);
    if(ret != 0)
        file_error("unable to peek at port %s (%s)", s_str(port_name(p)), strerror(errno));
    return b;
}

static scmval file_char_ready(scmval p) {
    if(is_port_open(p))
        return scm_true; // XXX is this enough ?
    return scm_false;
}

static void file_putc(scmval op, scmval v) {
    FILE* fp = port_data(op);
    fputc(c_char(v), fp);
}

static void file_puts(scmval op, scmval v) {
    FILE* fp = port_data(op);
    CORD_fprintf(fp, "%r", c_str(v));
}

static void file_putb(scmval p, scmval v) {
    FILE* fp = port_data(p);
    byte b[1] = { c_fix(v) };
    size_t size = fwrite(b, sizeof(byte), 1, fp);
    if(size != 1) {
        if(ferror(fp))
            file_error("unable to write byte to port %s (%s)", s_str(port_name(p)), strerror(errno));
    }
}

static void file_flush(scmval op) {
    FILE* fp = port_data(op);
    fflush(fp);
}

static scmval make_file_input_port(FILE* fp, char* name, short mode) {
    static scm_port_vtable_t vtable =
      { file_close, file_getc, file_ungetc, file_getb, file_peekb, file_char_ready, NULL, NULL, NULL, NULL };
    scmval p = make_port(scm_port_input | scm_port_file | mode, fp, name, &vtable);
    return p;
}

static scmval make_file_input_port_from_filename(scmval f) {
    FILE* fp = fopen(c_str(f), "r");
    if(fp == NULL) 
        file_error("unable to open input port for file %s (%s)", f, strerror(errno));
    return make_file_input_port(fp, c_cstr(f), scm_port_text);
}

static scmval make_binary_file_input_port_from_filename(scmval f) {
    FILE* fp = fopen(c_str(f), "rb");
    if(fp == NULL) 
        file_error("unable to open binary input port for file %s (%s)", f, strerror(errno));
    return make_file_input_port(fp, c_cstr(f), scm_port_binary);
}

static scmval make_file_output_port(FILE* fp, char* name, short mode) {
    static scm_port_vtable_t vtable =
      { file_close, NULL, NULL, NULL, NULL, file_char_ready, file_putc, file_puts, file_putb, file_flush };
    return make_port(scm_port_output | scm_port_file | mode, fp, name, &vtable);
}

static scmval make_file_output_port_from_filename(scmval f) {
    FILE* fp = fopen(c_str(f), "w");
    if(fp == NULL) 
        file_error("unable to open output port for file %s (%s)", f, strerror(errno));
    return make_file_output_port(fp, c_cstr(f), scm_port_text);
}

static scmval make_binary_file_output_port_from_filename(scmval f) {
    FILE* fp = fopen(c_str(f), "wb");
    if(fp == NULL) 
        file_error("unable to open output port for file %s (%s)", f, strerror(errno));
    return make_file_output_port(fp, c_cstr(f), scm_port_binary);
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
    return s_char(c);
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
    return s_bool(in->idx >= in->len);
}

static void string_putc(scmval p, scmval v) {
    scm_string_t* s = port_data(p);
    CORD c = CORD_chars(c_char(v), 1);
    s->value = CORD_cat(s->value, c);
}

static void string_puts(scmval p, scmval v) {
    scm_string_t* s = port_data(p);
    s->value = CORD_cat(s->value, c_str(v));
}

static void string_flush(scmval p) {
}

static scmval scm_str_input_port(scmval s) {
    static scm_port_vtable_t vtable =
      { string_close, string_getc, string_ungetc, NULL, NULL, string_char_ready, NULL, NULL, NULL, NULL };
    scm_input_string_t* in = scm_new(scm_input_string_t);
    in->buf = CORD_to_const_char_star(c_str(s));
    in->idx = 0;
    in->len = strlen(in->buf);
    return make_port(scm_port_input | scm_port_string | scm_port_text, in, "string", &vtable);
}

static scmval scm_str_output_port() {
    static scm_port_vtable_t vtable =
      { string_close, NULL, NULL, NULL, NULL, NULL, string_putc, string_puts, NULL, string_flush };
    scm_string_t* s = scm_new(scm_string_t);
    s->value = NULL;
    return make_port(scm_port_output | scm_port_string | scm_port_text, s, "string", &vtable);
}

////////////////////////////////////////////////////////////////////////////////
// -- BYTEVECTOR
static void bytevector_close(scmval p) {
    if(is_input_port(p))
        scm_delete(port_data(p));
    set_port_open(p, false);
}

static scmval bytevector_getb(scmval p) {
    scm_bytes_buffer_t* in = port_data(p);
    if(in->idx >= in->len)
        return scm_eof;
    byte b = in->data[in->idx++];
    return s_fix(b);
}

static scmval bytevector_peekb(scmval p) {
    scm_bytes_buffer_t* in = port_data(p);
    if(in->idx >= in->len)
        return scm_eof;
    return s_fix(in->data[in->idx]);
}

static scmval bytevector_ready(scmval p) {
    return scm_true;
}

static void bytevector_putb(scmval p, scmval b) {
    scm_bytes_buffer_t* out = port_data(p);
    ++out->idx;
    if(out->idx >= out->len) {
        out->len = 1.5*out->len;
        out->data = GC_REALLOC(out->data, out->len * sizeof(byte));
    }
    out->data[out->idx] = c_fix(b);
}

static void bytevector_flush(scmval p) {
}

static scmval make_bytevector_input_port(scmval bv) {
    static scm_port_vtable_t vtable =
      { bytevector_close, NULL, NULL, bytevector_getb, bytevector_peekb, bytevector_ready, NULL, NULL, NULL, NULL };
    scm_bytes_buffer_t* in = scm_new(scm_bytes_buffer_t);
    in->data = get_bytevector(bv)->elts;
    in->idx  = 0;
    in->len  = bytevector_size(bv);
    return make_port(scm_port_input | scm_port_bytevector | scm_port_binary, in, "bytevector", &vtable);
}

static scmval make_bytevector_output_port() {
    static scm_port_vtable_t vtable =
      { bytevector_close, NULL, NULL, NULL, NULL, bytevector_ready, NULL, NULL, bytevector_putb, bytevector_flush };
    scm_bytes_buffer_t* out = scm_new(scm_bytes_buffer_t);
    out->data = scm_new_atomic(DEFAULT_BYTEVECTOR_PORT_SIZE, byte);
    out->len  = DEFAULT_BYTEVECTOR_PORT_SIZE;
    out->idx  = -1;
    return make_port(scm_port_output | scm_port_bytevector | scm_port_binary, out, "bytevector", &vtable);
}

