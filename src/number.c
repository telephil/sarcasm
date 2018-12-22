#include "scm.h"

// globals
scmval scm_0;

// constructors
scmval scm_fix(fixnum i) {
    scmval v = make_val(SCM_TYPE_FIXNUM);
    v.i = i;
    return v;
}

scmval scm_flo(flonum d) {
    scmval v = make_val(SCM_TYPE_FLONUM);
    v.d = d;
    return v;
}

void init_number() {
    scm_0 = scm_fix(0);
}

