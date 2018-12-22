typedef struct scm_pair scm_pair_t;

struct scm_pair {
    scmval car;
    scmval cdr;
};

// globals
extern scmval scm_null;

// constructor
scmval make_pair(scmval car, scmval cdr);

// predicates
static inline bool is_null(scmval v) { return type_of(v) == SCM_TYPE_NULL; }
static inline bool is_pair(scmval v) { return is_null(v) || (type_of(v) == SCM_TYPE_PAIR); }

// contracts
define_contract(list_c, "list", is_pair);

// accessors
static inline scm_pair_t* get_pair(scmval v) { return (scm_pair_t*)v.o; }

// standard lib
void init_pair();

static inline scmval car(scmval v) { return get_pair(v)->car; }
static inline scmval cdr(scmval v) { return get_pair(v)->cdr; }
static inline void setcar(scmval v, scmval c) { get_pair(v)->car = c; }
static inline void setcdr(scmval v, scmval c) { get_pair(v)->cdr = c; }
static inline scmval cons(scmval car, scmval cdr) { return make_pair(car, cdr); }

#define cadr(V) car(cdr(V))
#define caddr(V) car(cdr(cdr(V)))
#define cadddr(V) car(cdr(cdr(cdr(V))))
#define cddr(V) cdr(cdr(V))
#define cdddr(V) cdr(cdr(cdr(V)))

static inline size_t list_length(scmval v) {
    size_t l;
    for(l = 0; !is_null(v); l++, v = cdr(v)) {
        if(!is_pair(cdr(v))) {
            l += 2;
            break;
        }
    }
    return l;
} 
