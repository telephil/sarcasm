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
void init_symbol(scmval);

// intern pool
void init_intern_pool();
scmval intern(const char*);

#define define_symbol(CNAME,SNAME) extern scmval CNAME;
#include "symbols.inc"
#undef define_symbol



