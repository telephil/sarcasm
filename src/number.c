#include "scm.h"

// constructors
scmval make_fixnum(scm_fixnum_t i) {
    scmval v = make_val(SCM_TYPE_FIXNUM);
    v.i = i;
    return v;
}

scmval make_flonum(scm_flonum_t d) {
    scmval v = make_val(SCM_TYPE_FLONUM);
    v.d = d;
    return v;
}

