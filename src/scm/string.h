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

// accessors
static inline scm_string_t* get_string(scmval v) { return (scm_string_t*)v.o; }
static inline char* get_string_cstr(scmval v) { return CORD_to_char_star(get_string(v)->value); }
