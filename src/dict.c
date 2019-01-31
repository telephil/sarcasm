#include <gc/gc.h>
#include "scm.h"

#define kcalloc(N,Z) kmalloc(N*Z)
#define kmalloc(Z) GC_MALLOC(Z)
#define krealloc(P,Z) GC_REALLOC(P,Z)
#define kfree(P) GC_FREE(P)

#include "khash.h"

static inline khint_t scm_symbol_hash(scmval v) {
    return kh_str_hash_func(c_str(v));
}

static inline bool scm_symbol_equal(scmval s1, scmval s2) {
    return string_equal_p(s1, s2);
}

KHASH_INIT(dict, scmval, scmval, 1, scm_symbol_hash, scm_symbol_equal)

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
}

scmval dict_maybe_set(scm_dict_t* d, scmval k, scmval v) {
    int ret;
    khiter_t it;
    it = kh_put(dict, d, k, &ret);
    if(ret == 0) { // key already exist -> return it
        return kh_value(d, it);
    }
    kh_value(d, it) = v;
    return v;
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

void dict_copy(scm_dict_t* dest, scm_dict_t* src) {
    scmval k, v;
    kh_foreach(src, k, v, dict_set(dest, k, v));
}

void dict_foreach(scm_dict_t* dict, void(*callback)(scmval,scmval,scmval), scmval data) {
    scmval k, v;
    kh_foreach(dict, k, v, callback(k, v, data));
}
