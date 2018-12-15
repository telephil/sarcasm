typedef struct scm_vector scm_vector_t;

struct scm_vector {
    size_t  size;
    scmval* elts;
};

// constructor
scmval make_vector(size_t, scmval);
scmval make_vector_from_list(int, scmval);

// accessor
static inline scm_vector_t* get_vector(scmval v) { return (scm_vector_t*)v.o; }

// predicate
static inline bool is_vector(scmval v) { return type_of(v) == SCM_TYPE_VECTOR; }

// contracts
define_contract(vector_c, "vector", is_vector);

// standard lib
void init_vector(scm_ctx_t*);

static inline size_t vector_size(scmval v) { return get_vector(v)->size; }
static inline scmval vector_ref(scmval v, int i) { return get_vector(v)->elts[i]; }
static inline void vector_set(scmval v, int i, scmval a) { get_vector(v)->elts[i] = a; }

