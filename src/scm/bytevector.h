typedef struct scm_bytevector scm_bytevector_t;

struct scm_bytevector {
    size_t   size;
    byte* elts;
};

// constructor
scmval make_bytevector(size_t, scmval);
scmval make_bytevector_from_list(int, scmval);
scmval make_bytevector_from_data(int, byte*);
// accessors
static inline scm_bytevector_t* get_bytevector(scmval v) { return (scm_bytevector_t*)v.o; }
static inline size_t  bytevector_size(scmval v) { return get_bytevector(v)->size; }
static inline scmval  bytevector_ref(scmval v, int i) { return s_fix(get_bytevector(v)->elts[i]); }
static inline void    bytevector_set(scmval v, int i, scmval x) { get_bytevector(v)->elts[i] = c_fix(x); }
// predicate
static inline bool is_bytevector(scmval v) { return type_of(v) == SCM_TYPE_BYTEVECTOR; }
static inline bool is_byte(scmval v) { return is_fixnum(v) && c_fix(v) >= 0 && c_fix(v) <= 255; }
// contract
define_contract(bytevector_c, "bytevector", is_bytevector);
define_contract(byte_c, "byte", is_byte);
// standard library
void init_bytevector(scmval);

