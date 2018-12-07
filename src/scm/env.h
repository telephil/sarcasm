typedef struct scm_env scm_env_t;

struct scm_env {
    scm_dict_t* bindings;
    scm_env_t* next;
};

// constructor
scmval make_env();

// predicates
static inline bool is_env(scmval v) { return type_of(v) == SCM_TYPE_ENV; }

// accessors
static inline scm_env_t* get_env(scmval v) { return (scm_env_t*)v.o; }
static inline scm_dict_t* env_bindings(scmval v) { return get_env(v)->bindings; }

// standard library
void define(scm_ctx_t*, const char*, scm_prim_fun, arity_t, int, ...);
scmval lookup(scm_ctx_t*, scmval);

