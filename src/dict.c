#include <gc/gc.h>
#include "scm.h"

#define kcalloc(N,Z) kmalloc(N*Z)
#define kmalloc(Z) GC_MALLOC(Z)
#define krealloc(P,Z) GC_REALLOC(P,Z)
#define kfree(P) GC_FREE(P)

#include "khash.h"

#define kh_scm_str_hash(a) (kh_str_hash_func(string_to_cstr(a)))
#define kh_scm_str_cmp(a, b) (kh_str_hash_equal(string_to_cstr(a), string_to_cstr(b)))

KHASH_INIT(dict, scmval, scmval, 1, kh_scm_str_hash, kh_scm_str_cmp) //;

scm_dict_t* make_dict() {
    scm_dict_t* dict = kh_init(dict);
    return dict;
}

void dict_set(scm_dict_t* d, scmval k, scmval v) {
    int ret;
    khiter_t it;
    it = kh_put(dict, d, k, &ret);
    // TODO ret
    kh_value(d, it) = v;
//    printf("*** added symbol '%s' to globals\n", string_to_cstr(k));
}

scmval dict_ref(scm_dict_t* d, scmval k) {
    khiter_t it;
    it = kh_get(dict, d, k);
    if(it == kh_end(d)) {
        return scm_undef;
    }
    scmval r = kh_value(d, it);
    return r;
}

