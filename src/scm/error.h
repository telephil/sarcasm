typedef struct scm_error scm_error_t;

struct scm_error {
    scmval type;
    scmval message;
};

// constructor
scmval make_error(scmval type, scmval message);

// predicates
static inline bool is_error(scmval v) { return type_of(v) == SCM_TYPE_ERROR; }

// accessors
static inline scm_error_t* get_error(scmval v) { return (scm_error_t*)v.o; }
static inline scmval error_type(scmval v) { return get_error(v)->type; }
static inline scmval error_message(scmval v) { return get_error(v)->message; }

// standard library
void init_errors();
void raise(scmval);

static inline void error(scmval type, const char* format, ...) {
    char* buf;
    va_list ap;
    va_start(ap, format);
    vasprintf(&buf, format, ap);
    va_end(ap);
    scmval e = make_error(type, make_string(buf));
    raise(e);
}

// exceptions
#define with_error_handler(F)               \
    if(setjmp(scm_context.err_buf)) {    \
        F(scm_context.err);              \
    } else                                  

// common error types
extern scmval type_error_type;
extern scmval range_error_type;
extern scmval arity_error_type;

static inline void range_error(const char* name, int index, int size) {
    error(range_error_type,
          "%s: index out of range (index: %d - valid range: [0;%d])",
          name, index, size-1);
}

static inline void range_error2(const char* name, int index, int low, int high) {
    error(range_error_type,
          "%s: index out of range (index: %d - valid range: [%d;%d])",
          name, index, low, high-1);
}

