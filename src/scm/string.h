#include <gc/cord.h>

typedef struct scm_string scm_string_t;

struct scm_string {
    CORD value;
};

// constructors
scmval make_string(const char*);
scmval make_string_from_cord(CORD);

// predicates
static inline bool is_string(scmval v) { return type_of(v) == SCM_TYPE_STRING; }

// contract
define_contract(string_c, "string", is_string);

// accessors
static inline scm_string_t* get_string(scmval v) { return (scm_string_t*)v.o; }
static inline CORD          string_value(scmval v) { return get_string(v)->value; }
static inline scmfix        string_length(scmval v) { return CORD_len(string_value(v)); }
static inline char*         string_to_cstr(scmval v) { return CORD_to_char_star(string_value(v)); }

// standard library
void init_string();
