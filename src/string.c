#include "scm.h"

// constructor
scmval make_string(const char* s) {
    scm_string_t* c = scm_new(scm_string_t);
    c->value = CORD_from_char_star(s);
    return make_ptr(SCM_TYPE_STRING, c);
}

scmval make_string_from_cord(CORD c) {
    scm_string_t* s = scm_new(scm_string_t);
    s->value = c;
    return make_ptr(SCM_TYPE_STRING, s);
}

