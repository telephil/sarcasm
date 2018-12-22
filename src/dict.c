#include <gc/gc.h>
#include "scm.h"

#define kcalloc(N,Z) kmalloc(N*Z)
#define kmalloc(Z) GC_MALLOC(Z)
#define krealloc(P,Z) GC_REALLOC(P,Z)
#define kfree(P) GC_FREE(P)

#include "khash.h"

#define kh_scm_str_hash(a) (kh_str_hash_func(c_cstr(a)))
#define kh_scm_str_cmp(a, b) (kh_str_hash_equal(c_cstr(a), c_cstr(b)))

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
//    printf("*** added symbol '%s' to globals\n", c_cstr(k));
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

void dict_keys(scm_dict_t* d, int* size, scmval** keys) {
    int i = 0;
    int s = kh_size(d);
    scmval* res = scm_new_array(s, scmval);
    scmval k, v;
    kh_foreach(d, k, v, res[i++] = k);
    *size = s;
    *keys = res;
}

