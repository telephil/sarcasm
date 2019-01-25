typedef struct arity    arity_t;

enum arity_type { ARITY_EXACTLY, ARITY_OR, ARITY_BETWEEN, ARITY_AT_LEAST };

struct arity {
    enum arity_type type;
    int min;
    int max;
};

static inline arity_t arity_exactly(int n) { arity_t a = { ARITY_EXACTLY, n, 0 }; return a; }
static inline arity_t arity_or(int n, int m) { arity_t a = { ARITY_OR, n, m }; return a; }
static inline arity_t arity_between(int n, int m) { arity_t a = { ARITY_BETWEEN, n, m }; return a; }
static inline arity_t arity_at_least(int n) { arity_t a = { ARITY_AT_LEAST, n, 0 }; return a; }


