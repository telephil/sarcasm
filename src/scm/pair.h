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
static inline bool is_list(scmval v) { return type_of(v) == SCM_TYPE_PAIR; }

// contracts
define_contract(pair_c, "pair", is_pair); // XXX should be list_c
define_contract(list_c, "list", is_list); // XXX should be pair_c

// accessors
static inline scm_pair_t* get_pair(scmval v) { return (scm_pair_t*)v.o; }

// standard lib
void init_pair(scmval);

static inline scmval car(scmval v) { return get_pair(v)->car; }
static inline scmval cdr(scmval v) { return get_pair(v)->cdr; }
static inline void setcar(scmval v, scmval c) { get_pair(v)->car = c; }
static inline void setcdr(scmval v, scmval c) { get_pair(v)->cdr = c; }
static inline scmval cons(scmval car, scmval cdr) { return make_pair(car, cdr); }

#define foreach(E,L) for(scmval E, _lst=L; !is_null(_lst)&&!is_undef(E=car(_lst));_lst=cdr(_lst))

#define caar(V) car(car(V))
#define cadr(V) car(cdr(V))
#define cdar(V) cdr(car(V))
#define cddr(V) cdr(cdr(V))
#define caddr(V) car(cdr(cdr(V)))
#define cdddr(V) cdr(cdr(cdr(V)))
#define cadddr(V) car(cdr(cdr(cdr(V))))
#define cddddr(V) cdr(cdr(cdr(cdr(V))))

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

static inline scmval list1(scmval e) { return cons(e, scm_null); }
static inline scmval list2(scmval e1, scmval e2) { return cons(e1, list1(e2)); }
static inline scmval list3(scmval e1, scmval e2, scmval e3) { return cons(e1, list2(e2, e3)); }
static inline scmval list4(scmval e1, scmval e2, scmval e3, scmval e4) { return cons(e1, list3(e2, e3, e4)); }

