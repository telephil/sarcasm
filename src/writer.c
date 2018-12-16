#include "scm.h"

static void write_char(scmval, scmval, write_mode);
static void write_pair(scmval, scmval, write_mode);
static void write_vector(scmval, scmval, write_mode);

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
                write_char(p, v, mode);
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
            write_pair(p, v, mode);
            break;
        case SCM_TYPE_VECTOR:
            write_vector(p, v, mode);
            break;
        case SCM_TYPE_ENV:
            scm_puts(p, "#<environment>");
            break;
        case SCM_TYPE_PRIM:
            scm_printf(p, "#<primitive:%s>", prim_name(v));
            break;
        case SCM_TYPE_ERROR:
            scm_putc(p, '[');
            write(p, error_type(v), mode);
            scm_puts(p, "] ");
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

static void write_char(scmval p, scmval v, write_mode mode) {
    scm_char_t c = char_value(v);
    scm_puts(p, "#\\");
    switch(c) {
        case '\a':
            scm_puts(p, "alarm");
            break;
        case '\b':
            scm_puts(p, "backspace");
            break;
        case '\n':
            scm_puts(p, "newline");
            break;
        case '\0':
            scm_puts(p, "null");
            break;
        case '\r':
            scm_puts(p, "return");
            break;
        case ' ':
            scm_puts(p, "space");
            break;
        case '\t':
            scm_puts(p, "tab");
            break;
        case 0xb:
            scm_puts(p, "vtab");
            break;
        default:
            scm_putc(p, c);
            break;
    }
}

static void write_pair(scmval p, scmval v, write_mode mode) {
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
}

static void write_vector(scmval p, scmval v, write_mode mode) {
    scm_puts(p, "#(");
    for(int i = 0; i < vector_size(v); i++) {
        if(i > 0) {
            scm_putc(p, ' ');
        }
        write(p, vector_ref(v, i), mode);
    }
    scm_putc(p, ')');
}

