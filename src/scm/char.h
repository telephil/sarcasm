// constructor
scmval make_char(scm_char_t);
// predicate
static inline bool is_char(scmval v) { return type_of(v) == SCM_TYPE_CHAR; }
// contract
define_contract(char_c, "character", is_char);
// accessor
static inline scm_char_t char_value(scmval v) { return v.c; }
// standard library
void init_char(scm_ctx_t*);
