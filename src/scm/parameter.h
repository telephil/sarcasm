typedef struct scm_parameter scm_parameter_t;

struct scm_parameter {
    scmval value;
    scmval converter;
};

// constructor
scmval make_parameter(scmval, scmval);

// predicate
static inline bool is_parameter(scmval v) { return type_of(v) == SCM_TYPE_PARAMETER; }

// contract
define_contract(parameter_c, "parameter", is_parameter);

// accessors
static inline scm_parameter_t*  get_parameter(scmval v) { return (scm_parameter_t*)v.o; }
static inline scmval parameter_conv(scmval p) { return get_parameter(p)->converter; }
scmval parameter_value(scmval p);
void   set_parameter_value(scmval p, scmval v);

// standard library
void init_parameter(scmval);

void dynenv_push_frame(scmval);
void dynenv_pop_frame(scmval);

