typedef struct scm_env scm_env_t;

struct scm_env {
    scm_dict_t* bindings;
    scmval next;
};

// constructor
scmval make_env(scmval);

// predicates
static inline bool is_env(scmval v) { return type_of(v) == SCM_TYPE_ENV; }
// contract
define_contract(env_c,  "environment",  is_env);

// accessors
static inline scm_env_t* get_env(scmval v) { return (scm_env_t*)v.o; }
static inline scm_dict_t* env_bindings(scmval v) { return get_env(v)->bindings; }
static inline scmval env_next(scmval v) { return get_env(v)->next; }

// standard library
void define(const char*, subr_f, arity_t);
scmval lookup(scmval, scmval);
void bind(scmval, scmval, scmval);

