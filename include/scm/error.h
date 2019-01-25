typedef struct scm_error scm_error_t;

struct scm_error {
    scmval type;
    scmval message;
    scmval irritants;
};

// globals
extern scmval   scm_g_lasterr;
extern jmp_buf  scm_g_errbuf;

// constructor
scmval make_error(scmval, scmval, scmval);

// predicates
static inline bool is_error(scmval v) { return type_of(v) == SCM_TYPE_ERROR; }

// contract
define_contract(error_c, "error", is_error);

// accessors
static inline scm_error_t* get_error(scmval v) { return (scm_error_t*)v.o; }
static inline scmval error_type(scmval v) { return get_error(v)->type; }
static inline scmval error_message(scmval v) { return get_error(v)->message; }
static inline scmval error_irritants(scmval v) { return get_error(v)->irritants; }

// standard library
void init_errors(scmval);
void raise(scmval);

static inline void error(scmval type, const char* format, ...) {
    char* buf;
    va_list ap;
    va_start(ap, format);
    vasprintf(&buf, format, ap);
    va_end(ap);
    scmval e = make_error(type, s_str(buf), scm_null);
    raise(e);
}

// exceptions
#define with_error_handler(F)   \
    if(setjmp(scm_g_errbuf)) {  \
        F(scm_g_lasterr);       \
    } else

// common error types
extern scmval exn_error_type;
extern scmval cexn_error_type;
extern scmval type_error_type;
extern scmval range_error_type;
extern scmval arity_error_type;
extern scmval file_error_type;

static inline void range_error(const char* name, int index, int low, int high) {
    error(range_error_type,
          "%s: index out of range (index: %d - valid range: [%d;%d])",
          name, index, low, high-1);
}

