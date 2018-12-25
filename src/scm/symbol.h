// globals 
extern scmval scm_undef;
extern scmval scm_void;

// constructors
scmval make_symbol(const char*);

// predicates
static inline bool is_symbol(scmval v) { return type_of(v) == SCM_TYPE_SYMBOL; }
static inline bool is_undef(scmval v) { return type_of(v) == SCM_TYPE_UNDEF; }
static inline bool is_void(scmval v) { return type_of(v) == SCM_TYPE_VOID; }

// contracts
define_contract(symbol_c, "symbol", is_symbol);

// accessors
static inline scm_string_t* get_symbol(scmval v) { return (scm_string_t*)v.o; }

// initialization
void init_symbol();

// standard library
scmval intern(const char*);


