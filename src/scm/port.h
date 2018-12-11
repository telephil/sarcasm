typedef struct scm_output_port scm_output_port_t;
typedef struct outp_vtable outp_vtable;

// output port
enum port_type { FILE_PORT, STRING_PORT };

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
static inline scm_output_port_t* get_output_port(scmval v) { return (scm_output_port_t*)v.o; }
static inline enum port_type output_port_type(scmval v) { return get_output_port(v)->type; }
static inline void* output_port_port(scmval v) { return get_output_port(v)->port; }
static inline bool is_port_open(scmval v) { return get_output_port(v)->open; }
static inline void set_port_open(scmval p, bool b) { get_output_port(p)->open = b; } 
static inline void output_port_putc(scmval p, scmval v) { get_output_port(p)->vtable->putc(p, v); }
static inline void output_port_puts(scmval p, scmval v) { get_output_port(p)->vtable->puts(p, v); }
static inline void output_port_flush(scmval p) { get_output_port(p)->vtable->flush(p); }
static inline void output_port_close(scmval p) { get_output_port(p)->vtable->close(p); }


// standard library
void init_port(scm_ctx_t*);
scmval open_output_string();
scmval get_output_string(scmval);
