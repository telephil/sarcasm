#include "scm.h"

static CORD to_cord(scmval v, bool write) {
    CORD r;
    switch(type_of(v)) {
        case SCM_TYPE_UNDEF:
            r = "#<undefined>";
            break;
        case SCM_TYPE_NULL:
            r = "()";
            break;
        case SCM_TYPE_EOF:
            r = "#<eof>";
            break;
        case SCM_TYPE_BOOL:
            r = is_true(v) ? "#t" : "#f";
            break;
        case SCM_TYPE_FIXNUM:
            CORD_sprintf(&r, "%lld", fixnum_value(v));
            break;
        case SCM_TYPE_FLONUM:
            CORD_sprintf(&r, "%lf", flonum_value(v));
            break;
        case SCM_TYPE_CHAR:
            if(write) {
                CORD_sprintf(&r, "#\\%c", char_value(v));
            } else {
                CORD_sprintf(&r, "%c", char_value(v));
            }
            break;
        case SCM_TYPE_SYMBOL:
            r = string_value(v);
            break;
        case SCM_TYPE_PAIR:
            r = "(";
            for(scmval l = v; !is_null(l); l = cdr(l)) {
                CORD c = to_cord(car(l), write);
                r = CORD_cat(r, c);
                if(!is_pair(cdr(l))) {
                    CORD c = to_cord(cdr(l), write); 
                    r = CORD_cat(r, " . ");
                    r = CORD_cat(r, c);
                    break;
                }
                if(!is_null(cdr(l))) {
                    r = CORD_cat(r, " ");
                }
            }
            r = CORD_cat(r, ")");
            break;
        case SCM_TYPE_VECTOR:
            r = "#(";
            for(int i = 0; i < vector_size(v); i++) {
                if(i > 0) {
                    r = CORD_cat(r, " ");
                }
                CORD s = to_cord(vector_ref(v, i), write);
                r = CORD_cat(r, s);
            }
            r = CORD_cat(r, ")");
            break;
        case SCM_TYPE_STRING:
            if(write)
                CORD_sprintf(&r, "\"%r\"", string_value(v));
            else
                r = string_value(v);
            break;
    }
    return r;
}

scmval to_str(scmval v, bool write) {
    CORD c = to_cord(v, write);
    return make_string_from_cord(c);
}

