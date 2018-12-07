#include "scm.h"

// constructor
scmval make_char(scm_char_t c) {
    scmval v = make_val(SCM_TYPE_CHAR);
    v.c = c;
    return v;
}

