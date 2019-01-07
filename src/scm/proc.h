typedef struct scm_primitive    scm_primitive_t;
typedef struct scm_closure      scm_closure_t;

typedef scmval (*primitive_f)();

// primitive
struct scm_primitive {
    scmval      name;
    primitive_f f;
    arity_t     arity;
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
scmval make_primitive(const char*, primitive_f, arity_t);
scmval make_closure(scmval, int, scmval*, scmval, scmval);

// predicate
static inline bool is_primitive(scmval v) { return type_of(v) == SCM_TYPE_PRIMITIVE; }
static inline bool is_closure(scmval v) { return type_of(v) == SCM_TYPE_CLOSURE; }
static inline bool is_procedure(scmval v) { return is_primitive(v) || is_closure(v); }
static inline bool is_callable(scmval v) { return is_primitive(v) || is_closure(v); }

// contract
define_contract(procedure_c, "procedure", is_procedure);

// accessors
static inline scm_primitive_t*  get_primitive(scmval v) { return (scm_primitive_t*)v.o; }
static inline scmval            primitive_name(scmval v) { return get_primitive(v)->name; }
static inline arity_t           primitive_arity(scmval v) { return get_primitive(v)->arity; }
#define funcall0(v)             get_primitive(v)->f()
#define funcall(v,...)          get_primitive(v)->f(__VA_ARGS__)

static inline scm_closure_t* get_closure(scmval v) { return (scm_closure_t*)v.o; }
static inline scmval         closure_name(scmval v) { return get_closure(v)->name; }
static inline int            closure_argc(scmval v) { return get_closure(v)->argc; }
static inline scmval*        closure_argv(scmval v) { return get_closure(v)->argv; }
static inline scmval         closure_env(scmval v)  { return get_closure(v)->env; }
static inline scmval         closure_body(scmval v) { return get_closure(v)->body; }
static inline void           set_closure_name(scmval v, scmval n) { get_closure(v)->name = n; }

// utils
void check_arity(scmval, int);
int argc_from_arity(scmval, int);

static inline void type_error(const char* name, contract_t c, scmval r) {
    error(type_error_type,
          "%s: contract violation (expected %s but received %s)",
          name, c.name, scm_to_cstr(r));
}

