// globals
extern scmval scm_0;
// constructors
scmval scm_fix(fixnum);
scmval scm_flo(flonum);
// predicates
static inline bool is_fixnum(scmval v) { return type_of(v) == SCM_TYPE_FIXNUM; }
static inline bool is_flonum(scmval v) { return type_of(v) == SCM_TYPE_FLONUM; }
// contracts
define_contract(fixnum_c, "integer", is_fixnum);
// accessors
static inline fixnum c_fix(scmval v) { return v.i; }
static inline flonum c_flo(scmval v) { return v.d; }
// standard library
void init_number();
