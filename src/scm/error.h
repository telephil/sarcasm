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
void init_errors(scm_ctx_t*);

static inline scmval error(scmval type, const char* format, ...) {
    char* buf;
    va_list ap;
    va_start(ap, format);
    vasprintf(&buf, format, ap);
    va_end(ap);
    return make_error(type, make_string(buf));
}

// exceptions
#define try(C) if(!setjmp(C->err_buf))
#define catch else
void throw(scm_ctx_t*, scmval);

// common error types
extern scmval range_error_type;
extern scmval arity_error_type;
extern scmval contract_error_type;

static inline scmval range_error(const char* name, int index, int size) {
    scmval e = error(range_error_type,
                     "%s: index out of range (index: %d - valid range: [0;%d])",
                     name, index, size);
    return e;
}

