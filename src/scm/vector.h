extern scm_type_t scm_type_vector;
// constructor
scmval make_vector(size_t, scmval);
scmval make_vector_from_list(int, scmval);
// accessor
size_t vector_size(scmval v);
scmval vector_ref(scmval v, int i);
void   vector_set(scmval v, int i, scmval a);
// predicate
static inline bool is_vector(scmval v) { return type_of(v) == scm_type_vector; }
// contracts
define_contract(vector_c, "vector", is_vector);
// standard lib
void init_vector(scmval);
scmval scm_vector_p(scmval v);
scmval scm_make_vector(scmval s, scmval i);
scmval scm_vector(int argc, scmval* argv);
scmval scm_vector_length(scmval vec);
scmval scm_vector_ref(scmval v, scmval i);
scmval scm_vector_set(scmval v, scmval i, scmval x);
scmval scm_vector_to_list(scmval v, scmval start, scmval end);
scmval scm_list_to_vector(scmval l);
scmval scm_vector_to_string(scmval v, scmval start, scmval end);
scmval scm_string_to_vector(scmval s, scmval start, scmval end);
scmval scm_vector_copy(scmval v, scmval start, scmval end);
scmval scm_vector_mcopy(scmval to, scmval at, scmval from, scmval start, scmval end);
scmval scm_vector_append(int argc, scmval* argv);
scmval scm_vector_fill(scmval v, scmval fill, scmval start, scmval end);


