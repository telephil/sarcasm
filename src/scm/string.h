#include <gc/cord.h>

typedef struct scm_string scm_string_t;

struct scm_string {
    CORD value;
};

// constructors
scmval scm_str(const char*);
scmval scm_str_from_cord(CORD);

// predicates
static inline bool is_string(scmval v) { return type_of(v) == SCM_TYPE_STRING; }

// contract
define_contract(string_c, "string", is_string);

// accessors
static inline scm_string_t* get_string(scmval v) { return (scm_string_t*)v.o; }
static inline CORD          c_str(scmval v) { return get_string(v)->value; }
static inline char*         c_cstr(scmval v) { return CORD_to_char_star(c_str(v)); }
static inline size_t        string_length(scmval v) { return CORD_len(c_str(v)); }

// standard library
void init_string(scmval);
