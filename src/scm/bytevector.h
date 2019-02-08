extern scm_type_t scm_type_bytevector;
// constructor
scmval make_bytevector(size_t, scmval);
scmval make_bytevector_from_list(int, scmval);
scmval make_bytevector_from_data(int, byte*);
// accessors
size_t bytevector_size(scmval);
byte*  bytevector_data(scmval);
scmval bytevector_ref(scmval, int);
void   bytevector_set(scmval, int, scmval);
// predicate
static inline bool is_bytevector(scmval v) { return type_of(v) == scm_type_bytevector; }
static inline bool is_byte(scmval v) { return is_fixnum(v) && c_fix(v) >= 0 && c_fix(v) <= 255; }
// contract
define_contract(bytevector_c, "bytevector", is_bytevector);
define_contract(byte_c, "byte", is_byte);
// standard library
void init_bytevector(scmval);
scmval scm_bytevector_p(scmval v);
scmval scm_make_bytevector(scmval size, scmval initial);
scmval scm_bytevector(int argc, scmval* argv);
scmval scm_bytevector_length(scmval b);
scmval scm_bytevector_ref(scmval b, scmval k);
scmval scm_bytevector_set(scmval b, scmval k, scmval byte);
scmval scm_bytevector_copy(scmval b, scmval start, scmval end);
scmval scm_bytevector_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end);
scmval scm_bytevector_append(int argc, scmval* argv);
scmval scm_utf8_to_string(scmval b, scmval start, scmval end);
scmval scm_string_to_utf8(scmval s, scmval start, scmval end);

#define define_type_predicate(CNAME,TYPE) \
    static inline bool CNAME(scmval v) { return type_of(v) == TYPE; }
