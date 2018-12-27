// globals
extern scmval scm_0;
extern scmval scm_pos_inf;
extern scmval scm_neg_inf;
extern scmval scm_nan;
// constructors
scmval scm_fix(fixnum);
scmval scm_flo(flonum);
// predicates
static inline bool is_fixnum(scmval v) { return type_of(v) == SCM_TYPE_FIXNUM; }
static inline bool is_flonum(scmval v) { return type_of(v) == SCM_TYPE_FLONUM; }
static inline bool is_number(scmval x) { return is_fixnum(x) || is_flonum(x); }
static inline bool is_nan(scmval x) { return is_eq(x, scm_nan); }
static inline bool is_pos_inf(scmval x) { return is_eq(x, scm_pos_inf); }
static inline bool is_neg_inf(scmval x) { return is_eq(x, scm_neg_inf); }
// contracts
define_contract(fixnum_c, "integer", is_fixnum);
define_contract(number_c, "number", is_number);

// accessors
static inline fixnum c_fix(scmval v) { return v.i; }
static inline flonum c_flo(scmval v) { return v.d; }
// standard library
void init_number();

