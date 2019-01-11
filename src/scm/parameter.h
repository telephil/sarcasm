typedef struct scm_parameter scm_parameter_t;

struct scm_parameter {
    scmval value;
};

// constructor
scmval make_parameter(scmval, scmval);

// predicate
static inline bool is_parameter(scmval v) { return type_of(v) == SCM_TYPE_PARAMETER; }

// contract
define_contract(parameter_c, "parameter", is_parameter);

// accessors
static inline scm_parameter_t*  get_parameter(scmval v) { return (scm_parameter_t*)v.o; }
static inline scmval            parameter_value(scmval v) { return get_parameter(v)->value; }
static inline void              set_parameter_value(scmval p, scmval v) { get_parameter(p)->value = v; }

// standard library
void init_parameter(scmval);
