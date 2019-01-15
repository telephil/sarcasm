typedef struct scm_port scm_port_t;
typedef struct scm_port_vtable  scm_port_vtable_t;
typedef struct scm_input_string scm_input_string_t;

enum {
    scm_port_input      = 1<<0,
    scm_port_output     = 1<<1,
    scm_port_file       = 1<<2,
    scm_port_string     = 1<<3,
    scm_port_bytevector = 1<<4,
    scm_port_text       = 1<<5,
    scm_port_binary     = 1<<6
};

struct scm_port {
    short   flags;
    void*   data;
    char*   name;
    bool    open;
    fixnum  line;
    fixnum  pos;
    scm_port_vtable_t* vtable;
};

struct scm_input_string {
    const char* buf;
    int idx;
    int len;
};

// vtable defs
typedef scmval  (*scm_port_getc)(scmval);
typedef scmval  (*scm_port_ungetc)(scmval, scmval);
typedef scmval  (*scm_port_getb)(scmval);
typedef scmval  (*scm_port_peekb)(scmval);
typedef scmval  (*scm_port_ready)(scmval);
typedef void    (*scm_port_close)(scmval);
typedef void    (*scm_port_putc)(scmval, scmval);
typedef void    (*scm_port_puts)(scmval, scmval);
typedef void    (*scm_port_putb)(scmval, scmval);
typedef void    (*scm_port_flush)(scmval);

struct scm_port_vtable {
    scm_port_close  close;
    scm_port_getc   getc;
    scm_port_ungetc ungetc;
    scm_port_getb   getb;
    scm_port_peekb  peekb;
    scm_port_ready  ready;
    scm_port_putc   putc;
    scm_port_puts   puts;
    scm_port_putb   putb;
    scm_port_flush  flush;
};

// globals
extern scmval scm_g_current_output_port;
extern scmval scm_g_current_error_port;
extern scmval scm_g_current_input_port;
extern scmval scm_eof;

// constructor
scmval make_port(short, void*, char*, scm_port_vtable_t*);

// accessors
static inline scm_port_t* get_port(scmval v) { return (scm_port_t*)v.o; }
static inline short       port_flags(scmval v) { return get_port(v)->flags; }
static inline void*       port_data(scmval v) { return get_port(v)->data; }
static inline char*       port_name(scmval v) { return get_port(v)->name; }
static inline bool        is_port_open(scmval v) { return get_port(v)->open; }
static inline void        set_port_open(scmval v, bool b) { get_port(v)->open = b; }
// macros to allow modification
#define port_line(P)      get_port(P)->line
#define port_pos(P)       get_port(P)->pos

// predicates
static inline bool is_eof(scmval v) { return type_of(v) == SCM_TYPE_EOF; }
static inline bool is_port(scmval v) { return type_of(v) == SCM_TYPE_PORT; }
static inline bool is_input_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_input); }
static inline bool is_output_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_output); }
static inline bool is_file_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_file); }
static inline bool is_string_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_string); }
static inline bool is_bytevector_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_bytevector); }
static inline bool is_output_string_port(scmval v) { return is_output_port(v) && is_string_port(v); }
static inline bool is_output_bytevector_port(scmval v) { return is_output_port(v) && is_bytevector_port(v); }
static inline bool is_textual_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_text); }
static inline bool is_textual_input_port(scmval v) { return is_input_port(v) && is_textual_port(v); }
static inline bool is_textual_output_port(scmval v) { return is_output_port(v) && is_textual_port(v); }
static inline bool is_binary_port(scmval v) { return is_port(v) && (port_flags(v) & scm_port_binary); }
static inline bool is_binary_input_port(scmval v) { return is_input_port(v) && is_binary_port(v); }
static inline bool is_binary_output_port(scmval v) { return is_output_port(v) && is_binary_port(v); }
// contracts
define_contract(port_c, "port", is_port);
define_contract(input_port_c, "input port", is_input_port);
define_contract(output_port_c, "output port", is_output_port);
define_contract(output_string_port_c, "output string port", is_output_string_port);
define_contract(output_bytevector_port_c, "output bytevector port", is_output_bytevector_port);
define_contract(textual_input_port_c, "textual input port", is_textual_input_port);
define_contract(textual_output_port_c, "textual output port", is_textual_output_port);
define_contract(binary_input_port_c, "binary input port", is_binary_input_port);
define_contract(binary_output_port_c, "binary output port", is_binary_output_port);

// port vtable accessors
static inline void   port_close(scmval p) { get_port(p)->vtable->close(p); }
static inline scmval port_getc(scmval p) { return get_port(p)->vtable->getc(p); }
static inline scmval port_ungetc(scmval p, scmval c) { return get_port(p)->vtable->ungetc(p, c); }
static inline scmval port_getb(scmval p) { return get_port(p)->vtable->getb(p); }
static inline scmval port_peekb(scmval p) { return get_port(p)->vtable->peekb(p); }
static inline scmval port_ready(scmval p) { return get_port(p)->vtable->ready(p); }
static inline void   port_putc(scmval p, scmval v) { get_port(p)->vtable->putc(p, v); }
static inline void   port_puts(scmval p, scmval v) { get_port(p)->vtable->puts(p, v); }
static inline void   port_putb(scmval p, scmval b) { get_port(p)->vtable->putb(p, b); }
static inline void   port_flush(scmval p) { get_port(p)->vtable->flush(p); }

// standard library
void   init_port(scmval);
scmval scm_read(scmval);
scmval scm_close_input_port(scmval);
scmval scm_open_input_file(scmval);
scmval scm_open_output_string();
scmval scm_get_output_string(scmval);
//
// utilities
scmval scm_current_output_port();
scmval scm_current_error_port();
scmval scm_to_string(scmval);
static inline CORD scm_to_cstr(scmval v) { return c_str(scm_to_string(v)); }

// C IO
char  scm_getc(scmval);
char  scm_peek(scmval);
void  scm_ungetc(scmval, char);
void  scm_putc(scmval, char);
void  scm_puts(scmval, CORD);
void  scm_printf(scmval, CORD, ...);

scmval open_input_string(const char*);

