typedef struct contract contract_t;
typedef bool (*predicate)(scmval);

struct contract {
    const char* name;
    predicate   pred;
};

#define define_contract(CNAME, NAME, PRED) static contract_t CNAME = { NAME, PRED }

// any/c
static inline bool is_any(scmval v) { return true; }
define_contract(any_c, "any/c", is_any);
