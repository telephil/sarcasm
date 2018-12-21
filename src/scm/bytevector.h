typedef struct scm_bytevector scm_bytevector_t;

struct scm_bytevector {
    size_t   size;
    scmbyte* elts;
};

// constructor
scmval make_bytevector(size_t, scmval);
scmval make_bytevector_from_list(scmfix, scmval);
// accessors
static inline scm_bytevector_t* get_bytevector(scmval v) { return (scm_bytevector_t*)v.o; }
static inline size_t  bytevector_size(scmval v) { return get_bytevector(v)->size; }
static inline scmval  bytevector_ref(scmval v, scmfix i) { return make_fixnum(get_bytevector(v)->elts[i]); }
static inline void    bytevector_set(scmval v, scmfix i, scmval x) { get_bytevector(v)->elts[i] = fixnum_value(x); }
// predicate
static inline bool is_bytevector(scmval v) { return type_of(v) == SCM_TYPE_BYTEVECTOR; }
static bool is_byte(scmval v) { return is_fixnum(v) && fixnum_value(v) >= 0 && fixnum_value(v) <= 255; }
// contract
define_contract(bytevector_c, "bytevector", is_bytevector);
define_contract(byte_c, "byte", is_byte);
// standard library
void init_bytevector();

