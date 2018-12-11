#include "scm.h"

static inline void scm_putc(scmval p, scm_char_t c) {
    output_port_putc(p, make_char(c));
}

static inline void scm_puts(scmval p, CORD c) {
    output_port_puts(p, make_string_from_cord(c));
}

static inline void scm_printf(scmval p, CORD format, ...) {
    CORD r;
    va_list ap;
    va_start(ap, format);
    CORD_vsprintf(&r, format, ap);
    va_end(ap);
    scm_puts(p, r);
}

void write(scmval p, scmval v, write_mode mode) {
    switch(type_of(v)) {
        case SCM_TYPE_UNDEF:
            scm_puts(p, "#<undefined>");
            break;
        case SCM_TYPE_NULL:
            scm_puts(p, "()");
            break;
        case SCM_TYPE_EOF:
            scm_puts(p, "#<eof>");
            break;
        case SCM_TYPE_BOOL:
            scm_puts(p, is_true(v) ? "#t" : "#f");
            break;
        case SCM_TYPE_FIXNUM:
            scm_printf(p, "%lld", fixnum_value(v));
            break;
        case SCM_TYPE_FLONUM:
            scm_printf(p, "%lf", flonum_value(v));
            break;
        case SCM_TYPE_CHAR:
            if(mode == WRITE_MODE_WRITE) {
                scm_puts(p, "#\\");
                scm_putc(p, char_value(v));
            } else {
                scm_putc(p, char_value(v));
            }
            break;
        case SCM_TYPE_STRING:
            if(mode == WRITE_MODE_WRITE) {
                scm_putc(p, '"');
                scm_puts(p, string_value(v));
                scm_putc(p, '"');
            } else {
                scm_puts(p, string_value(v));
            }
            break;
        case SCM_TYPE_SYMBOL:
            scm_puts(p, string_value(v));
            break;
        case SCM_TYPE_PAIR:
            scm_putc(p, '(');
            for(scmval l = v; !is_null(l); l = cdr(l)) {
                write(p, car(l), mode);
                if(!is_pair(cdr(l))) {
                    scm_puts(p, " . ");
                    write(p, cdr(l), mode);
                    break;
                }
                if(!is_null(cdr(l))) {
                    scm_putc(p, ' ');
                }
            }
            scm_putc(p, ')');
            break;
        case SCM_TYPE_VECTOR:
            scm_puts(p, "#(");
            for(int i = 0; i < vector_size(v); i++) {
                if(i > 0) {
                    scm_putc(p, ' ');
                }
                write(p, vector_ref(v, i), mode);
            }
            scm_putc(p, ')');
            break;
        case SCM_TYPE_ENV:
            scm_puts(p, "#<environment>");
            break;
        case SCM_TYPE_PRIM:
            scm_printf(p, "#<primitive:%s>", prim_name(v));
            break;
        case SCM_TYPE_ERROR:
            write(p, error_message(v), mode);
            break;
        case SCM_TYPE_INPUT_PORT:
            scm_puts(p, "#<input-port>"); // XXX
            break;
        case SCM_TYPE_OUTPUT_PORT:
            scm_printf(p, "#<output-port:%s>", output_port_type(v) == FILE_PORT ? "file" : "string");
            break;
    }
}

