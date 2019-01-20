typedef struct scm_pair scm_pair_t;

struct scm_pair {
    scmval car;
    scmval cdr;
};

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

static inline scmval list1(scmval e) { return cons(e, scm_null); }
static inline scmval list2(scmval e1, scmval e2) { return cons(e1, list1(e2)); }
static inline scmval list3(scmval e1, scmval e2, scmval e3) { return cons(e1, list2(e2, e3)); }
static inline scmval list4(scmval e1, scmval e2, scmval e3, scmval e4) { return cons(e1, list3(e2, e3, e4)); }

#define foreach(E,L) for(scmval E, _lst=L; !is_null(_lst)&&!is_undef(E=car(_lst));_lst=cdr(_lst))

#define push(V,L) L = cons(V, L)

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

static inline scmval list_tail(scmval lst, int k) {
    if(k == 0)
        return lst;
    return list_tail(cdr(lst), k - 1);
}

static inline scmval list_reverse(scmval lst) {
    if(is_null(lst))
        return scm_null;
    scmval res = list1(car(lst));
    foreach(elt, cdr(lst)) {
        push(elt, res);
    }
    return res;
}

static inline bool memq(scmval obj, scmval lst) {
    foreach(elt, lst) {
        if(is_eq(elt, obj))
            return true;
    }
    return false;
}

static inline scmval assq(scmval obj, scmval lst) {
    foreach(elt, lst) {
        if(is_eq(car(elt), obj)) {
            return elt;
        }
    }
    return scm_false;
}


typedef scmval(*f1)(scmval);
typedef scmval(*f2)(scmval, scmval);

static inline scmval map1(f1 f, scmval l) {
    scmval h = scm_null, t = h;
    foreach(elt, l) {
        scmval v = list1(f(elt));
        if(is_null(h)) {
            h = t = v;
        } else {
            setcdr(t, v);
            t = cdr(t);
        }
    }
    return h;
}

static inline scmval map2(f2 f, scmval l1, scmval l2) {
    scmval h = scm_null, t = h;
    while(!is_null(l1) && !is_null(l2)) {
        scmval e1 = car(l1);
        scmval e2 = car(l2);
        scmval v = list1(f(e1, e2));
        if(is_null(h)) {
            h = t = v;
        } else {
            setcdr(t, v);
            t = cdr(t);
        }
        l1 = cdr(l1);
        l2 = cdr(l2);
    }
    return h;
}

