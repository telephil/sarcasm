// constructor
scmval make_char(char);
// predicate
static inline bool is_char(scmval v) { return type_of(v) == SCM_TYPE_CHAR; }
// contract
define_contract(char_c, "character", is_char);
// accessor
static inline char c_char(scmval v) { return v.c; }
// standard library
void init_char(scmval);
