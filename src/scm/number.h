// constructors
scmval make_fixnum(scm_fixnum_t);
scmval make_flonum(scm_flonum_t);

// predicates
static inline bool is_fixnum(scmval v) { return type_of(v) == SCM_TYPE_FIXNUM; }
static inline bool is_flonum(scmval v) { return type_of(v) == SCM_TYPE_FLONUM; }

// contracts
define_contract(fixnum_c, "integer", is_fixnum);

// accessors
static inline scm_fixnum_t get_fixnum(scmval v) { return v.i; }
static inline scm_flonum_t get_flonum(scmval v) { return v.d; }
