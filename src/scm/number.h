// globals
extern scmval scm_0;
extern scmval scm_pos_inf;
extern scmval scm_neg_inf;
extern scmval scm_nan;
// constructors
scmval scm_fix(fixnum);
scmval scm_flo(flonum);
// accessors
static inline fixnum c_fix(scmval v) { return v.i; }
static inline flonum c_flo(scmval v) { return v.d; }
// predicates
static inline bool is_fixnum(scmval v) { return type_of(v) == SCM_TYPE_FIXNUM; }
static inline bool is_flonum(scmval v) { return type_of(v) == SCM_TYPE_FLONUM; }
static inline bool is_number(scmval x) { return is_fixnum(x) || is_flonum(x); }
static inline bool is_integer(scmval x) { return is_fixnum(x); }
static inline bool is_nan(scmval x) { return is_eq(x, scm_nan); }
static inline bool is_pos_inf(scmval x) { return is_eq(x, scm_pos_inf); }
static inline bool is_neg_inf(scmval x) { return is_eq(x, scm_neg_inf); }
static inline bool is_radix(scmval v) { fixnum i = c_fix(v); return i == 2 || i == 8 || i == 10 || i == 16; }
// contracts
define_contract(number_c, "number", is_number);
define_contract(fixnum_c, "integer", is_fixnum);
define_contract(integer_c, "integer", is_integer);
define_contract(radix_c, "a valid radix (2, 8, 10 or 16)", is_radix);
// standard library
void init_number();
scmval string_to_number(char*);
bool numeq(scmval x, scmval y);

