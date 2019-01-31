typedef struct scm_string scm_string_t;

struct scm_string {
    char* value;
};

// constructors
scmval s_str(const char*);
scmval s_str_nocopy(char*);

// predicates
static inline bool is_string(scmval v) { return type_of(v) == SCM_TYPE_STRING; }

// contract
define_contract(string_c, "string", is_string);

// accessors
static inline scm_string_t* get_string(scmval v) { return (scm_string_t*)v.o; }
static inline char*         c_str(scmval v) { return get_string(v)->value; }
static inline size_t        string_length(scmval v) { return strlen(c_str(v)); }
static inline bool          string_equal_p(scmval x, scmval y) { return !strcmp(c_str(x), c_str(y)); }
static inline char          string_ref(scmval s, int k) { return c_str(s)[k]; }
static inline void          string_set(scmval s, int k, char c) { c_str(s)[k] = c; }

// standard library
void init_string(scmval);
