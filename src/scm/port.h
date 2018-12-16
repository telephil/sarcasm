typedef struct scm_input_string scm_input_string_t;
typedef struct scm_input_port scm_input_port_t;
typedef struct ip_vtable ip_vtable;
typedef struct scm_output_port scm_output_port_t;
typedef struct outp_vtable outp_vtable;

enum port_type { FILE_PORT, STRING_PORT };

// input port
typedef scmval (*ip_getc)(scmval);
typedef scmval (*ip_ungetc)(scmval, scmval);
typedef scmval (*ip_char_ready)(scmval);
typedef void   (*ip_close)(scmval);

struct ip_vtable {
    ip_getc       getc;
    ip_ungetc     ungetc;
    ip_char_ready char_ready;
    ip_close      close;
};

struct scm_input_port {
    enum port_type type;
    void*          port;
    bool           open;
    int            line;
    int            pos;
    ip_vtable*     vtable;
};

struct scm_input_string {
    const char* buf;
    int idx;
    int len;
};

// output port
typedef void (*outp_putc)(scmval, scmval);
typedef void (*outp_puts)(scmval, scmval);
typedef void (*outp_flush)(scmval);
typedef void (*outp_close)(scmval);


struct outp_vtable {
    outp_putc  putc;
    outp_puts  puts;
    outp_flush flush;
    outp_close close;
};

struct scm_output_port {
    enum port_type type;
    void*          port;
    bool           open;
    outp_vtable*   vtable;
};

// globals
extern scmval scm_eof;

// constructors
scmval make_input_port(enum port_type, void*, ip_vtable*);
scmval make_output_port(enum port_type, void*, outp_vtable*);

// predicates
static inline bool is_eof(scmval v) { return type_of(v) == SCM_TYPE_EOF; }
static inline bool is_output_port(scmval v) { return type_of(v) == SCM_TYPE_OUTPUT_PORT; }
static inline bool is_input_port(scmval v) { return type_of(v) == SCM_TYPE_INPUT_PORT; }
static inline bool is_port(scmval v) { return is_output_port(v) || is_input_port(v); }

// contracts
define_contract(port_c, "port", is_port);
define_contract(input_port_c, "input port", is_input_port);
define_contract(output_port_c, "output port", is_output_port);


// accessors
static inline scm_input_port_t* get_input_port(scmval v) { return (scm_input_port_t*)v.o; }
static inline scm_output_port_t* get_output_port(scmval v) { return (scm_output_port_t*)v.o; }
static inline enum port_type input_port_type(scmval v) { return get_input_port(v)->type; }
static inline enum port_type output_port_type(scmval v) { return get_output_port(v)->type; }
static inline void* input_port_port(scmval v) { return get_input_port(v)->port; }
static inline void* output_port_port(scmval v) { return get_output_port(v)->port; }
static inline bool is_port_open(scmval v) { return get_output_port(v)->open; }
static inline void set_port_open(scmval p, bool b) {
    if(is_output_port(p)) get_output_port(p)->open = b;
    else                  get_input_port(p)->open = b;
} 
static inline scmval input_port_getc(scmval p) { return get_input_port(p)->vtable->getc(p); }
static inline scmval input_port_ungetc(scmval p, scmval c) { return get_input_port(p)->vtable->ungetc(p, c); }
static inline void input_port_close(scmval p) { get_input_port(p)->vtable->close(p); }
static inline void output_port_putc(scmval p, scmval v) { get_output_port(p)->vtable->putc(p, v); }
static inline void output_port_puts(scmval p, scmval v) { get_output_port(p)->vtable->puts(p, v); }
static inline void output_port_flush(scmval p) { get_output_port(p)->vtable->flush(p); }
static inline void output_port_close(scmval p) { get_output_port(p)->vtable->close(p); }
#define port_line(P) get_input_port(P)->line
#define port_pos(P) get_input_port(P)->pos


// standard library
void init_port(scm_ctx_t*);
//
// utilities
static inline scm_char_t scm_getc(scmval p) {
    scmval c = input_port_getc(p);
    if(is_eof(c))
        return EOF;
    if(char_value(c) == '\n') port_line(p)++;
    port_pos(p)++;
    return char_value(c);
}

static inline void scm_ungetc(scmval p, scm_char_t c) {
    input_port_ungetc(p, make_char(c));
    if(c == '\n') port_line(p)--;
    port_pos(p)--;
}

static inline scm_char_t scm_peek(scmval p) {
    scm_char_t c = scm_getc(p);
    input_port_ungetc(p, make_char(c));
    return c;
}
static inline void scm_putc(scmval p, scm_char_t c) { output_port_putc(p, make_char(c)); }
static inline void scm_puts(scmval p, CORD c) { output_port_puts(p, make_string_from_cord(c)); }

static inline void scm_printf(scmval p, CORD format, ...) {
    CORD r;
    va_list ap;
    va_start(ap, format);
    CORD_vsprintf(&r, format, ap);
    va_end(ap);
    scm_puts(p, r);
}

scmval open_input_string(const char*);
