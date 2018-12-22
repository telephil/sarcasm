typedef struct scm_subr scm_subr_t;
typedef struct scm_closure scm_closure_t;

typedef scmval (*subr_f)();

// subritive
struct scm_subr {
    scmval  name;
    subr_f  f;
    arity_t arity;
};

// closure
struct scm_closure {
    scmval  name;
    int     argc;
    scmval* argv;
    scmval  env;
    scmval  body;
};

// constructor
scmval make_subr(const char*, subr_f, arity_t);
scmval make_closure(scmval, int, scmval*, scmval, scmval);

// predicate
static inline bool is_subr(scmval v) { return type_of(v) == SCM_TYPE_SUBR; }
static inline bool is_closure(scmval v) { return type_of(v) == SCM_TYPE_CLOSURE; }
static inline bool is_callable(scmval v) { return is_subr(v) || is_closure(v); }

// accessors
static inline scm_subr_t*   get_subr(scmval v) { return (scm_subr_t*)v.o; }
static inline scmval        subr_name(scmval v) { return get_subr(v)->name; }
static inline arity_t       subr_arity(scmval v) { return get_subr(v)->arity; }
#define funcall0(v)         get_subr(v)->f()
#define funcall(v,...)      get_subr(v)->f(__VA_ARGS__)

static inline scm_closure_t* get_closure(scmval v) { return (scm_closure_t*)v.o; }
static inline scmval         closure_name(scmval v) { return get_closure(v)->name; }
static inline int            closure_argc(scmval v) { return get_closure(v)->argc; }
static inline scmval*        closure_argv(scmval v) { return get_closure(v)->argv; }
static inline scmval         closure_env(scmval v)  { return get_closure(v)->env; }
static inline scmval         closure_body(scmval v) { return get_closure(v)->body; }
static inline void           set_closure_name(scmval v, scmval n) { get_closure(v)->name = n; }

// utils
#define opt_arg(ARG,OPT) ARG = is_undef(ARG) ? OPT : ARG
#define check_arg(N,C,V) if(!C.pred(V)) (type_error(N,C,V))
#define check_args(N,C,AC,AV) for(int i = 0; i < AC; i++) { if(!C.pred(AV[i])) (type_error(N,C,AV[i])); }
#define check_range(N,V,L,H) if((V) < (L) || (V) >= (H)) range_error(N, (V), (L), (H))

// standard library
void check_arity(scmval, int);
int argc_from_arity(scmval, int);
scmval apply_funcall(scmval, int, scmval*);

static inline void type_error(const char* name, contract_t c, scmval r) {
    error(type_error_type,
          "%s: contract violation (expected %s but received %s)",
          name, c.name, scm_to_cstr(r));
}

