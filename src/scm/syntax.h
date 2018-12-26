typedef struct scm_syntax       scm_syntax_t;

struct scm_syntax {
    scmval  name;
    scmval  literals;
    scmval  rules;
};
// globals
extern scmval syntax_error_type;
extern scmval scm_underscore;
extern scmval scm_ellipsis;

// constructor
scmval make_syntax(scmval, scmval, scmval);

// predicates
static inline bool is_syntax(scmval v) { return type_of(v) == SCM_TYPE_SYNTAX; }

// contracts
define_contract(syntax_c,   "syntax",   is_syntax);

// accessors
static inline scm_syntax_t* get_syntax(scmval v) { return (scm_syntax_t*)v.o; }
static inline scmval        syntax_name(scmval v) { return get_syntax(v)->name; }
static inline scmval        syntax_literals(scmval v) { return get_syntax(v)->literals; }
static inline scmval        syntax_rules(scmval v) { return get_syntax(v)->rules; }

// standard library
void init_syntax();
scmval expand(scmval, scmval);
