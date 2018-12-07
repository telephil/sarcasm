typedef struct scm_prim scm_prim_t;

typedef scmval (*scm_prim_fun)(scm_ctx_t*);

// primitive
struct scm_prim {
    scmval name;
    scm_prim_fun fun;
    arity_t arity;
    contract_t* contracts;
};

// constructor
scmval make_prim(const char*, scm_prim_fun, arity_t, int, ...);
scmval make_primv(const char*, scm_prim_fun, arity_t, int, va_list);

// predicate
static inline bool is_prim(scmval v) { return type_of(v) == SCM_TYPE_PRIM; }

// accessors
static inline scm_prim_t* get_prim(scmval v) { return (scm_prim_t*)v.o; }

// standard library
scmval apply(scm_ctx_t*, scmval, scmval);
