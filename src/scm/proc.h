typedef struct scm_prim scm_prim_t;

typedef scmval (*scm_prim_fun)(scm_ctx_t*);

// primitive
struct scm_prim {
    scmval name;
    scm_prim_fun fun;
    arity_t arity;
    contract_t* contracts;
    int contract_count;
};

// constructor
scmval make_prim(const char*, scm_prim_fun, arity_t, int, ...);
scmval make_primv(const char*, scm_prim_fun, arity_t, int, va_list);

// predicate
static inline bool is_prim(scmval v) { return type_of(v) == SCM_TYPE_PRIM; }

// accessors
static inline scm_prim_t* get_prim(scmval v) { return (scm_prim_t*)v.o; }
static inline scmval prim_name(scmval v) { return get_prim(v)->name; }
static inline scm_prim_fun prim_fun(scmval v) { return get_prim(v)->fun; }
static inline arity_t prim_arity(scmval v) { return get_prim(v)->arity; }
static inline contract_t* prim_contracts(scmval v) { return get_prim(v)->contracts; }
static inline int prim_contract_count(scmval v) { return get_prim(v)->contract_count; }

// standard library
scmval apply(scm_ctx_t*, scmval, scmval);
