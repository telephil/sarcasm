typedef struct scm_prim scm_prim_t;
typedef struct scm_closure scm_closure_t;

typedef scmval (*scm_prim_fun)(scm_ctx_t*);

// primitive
struct scm_prim {
    scmval name;
    scm_prim_fun fun;
    arity_t arity;
    contract_t* contracts;
    int contract_count;
};

// closure
struct scm_closure {
    scm_fixnum_t argc;
    scmval*      argv;
    scmval       env;
    scmval       body;
};

// constructor
scmval make_prim(const char*, scm_prim_fun, arity_t, int, ...);
scmval make_primv(const char*, scm_prim_fun, arity_t, int, va_list);
scmval make_closure(scm_fixnum_t, scmval*, scmval, scmval);

// predicate
static inline bool is_prim(scmval v) { return type_of(v) == SCM_TYPE_PRIM; }
static inline bool is_closure(scmval v) { return type_of(v) == SCM_TYPE_CLOSURE; }
static inline bool is_callable(scmval v) { return is_prim(v) || is_closure(v); }

// accessors
static inline scm_prim_t* get_prim(scmval v) { return (scm_prim_t*)v.o; }
static inline scmval prim_name(scmval v) { return get_prim(v)->name; }
static inline scm_prim_fun prim_fun(scmval v) { return get_prim(v)->fun; }
static inline arity_t prim_arity(scmval v) { return get_prim(v)->arity; }
static inline contract_t* prim_contracts(scmval v) { return get_prim(v)->contracts; }
static inline int prim_contract_count(scmval v) { return get_prim(v)->contract_count; }

static inline scm_closure_t* get_closure(scmval v) { return (scm_closure_t*)v.o; }
static inline scm_fixnum_t   closure_argc(scmval v) { return get_closure(v)->argc; }
static inline scmval*        closure_argv(scmval v) { return get_closure(v)->argv; }
static inline scmval         closure_env(scmval v)  { return get_closure(v)->env; }
static inline scmval         closure_body(scmval v) { return get_closure(v)->body; }

// standard library
scmval apply(scm_ctx_t*, scmval, scmval);
