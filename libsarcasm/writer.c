#include "scm.h"

static void write_(scmval, scmval, short);
static void write_char(scmval, scmval, short);
static void write_pair(scmval, scmval, short);
static void write_vector(scmval, scmval, short);
static void write_bytevector(scmval, scmval, short);
static void write_error(scmval, scmval, short);
static void write_values(scmval, scmval, short);

enum {
    scm_mode_write      = 1<<0,
    scm_mode_display    = 1<<1,
    scm_mode_pp_quote   = 1<<2
};

void scm_write(scmval v, scmval p) {
    write_(p, v, scm_mode_write | scm_mode_pp_quote);
}

void scm_display(scmval v, scmval p) {
    write_(p, v, scm_mode_display | scm_mode_pp_quote);
}

static void write_(scmval p, scmval v, short flags) {
    switch(type_of(v)) {
        case SCM_TYPE_UNDEF:
            scm_puts(p, "#<undefined>");
            break;
        case SCM_TYPE_VOID:
            scm_puts(p, "#<void>");
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
        case SCM_TYPE_BIGNUM:
        case SCM_TYPE_FLONUM:
            scm_puts(p, c_str(number_to_string(v, s_fix(10))));
            break;
        case SCM_TYPE_CHAR:
            if(flags & scm_mode_write) {
                write_char(p, v, flags);
            } else {
                scm_putc(p, c_char(v));
            }
            break;
        case SCM_TYPE_STRING:
            if(flags & scm_mode_write) {
                scm_putc(p, '"');
                scm_puts(p, c_str(v));
                scm_putc(p, '"');
            } else {
                scm_puts(p, c_str(v));
            }
            break;
        case SCM_TYPE_SYMBOL:
            scm_puts(p, c_str(v));
            break;
        case SCM_TYPE_PAIR:
            if(has_flag(v, scm_flag_values))
                write_values(p, v, flags);
            else
                write_pair(p, v, flags);
            break;
        case SCM_TYPE_VECTOR:
            write_vector(p, v, flags);
            break;
        case SCM_TYPE_BYTEVECTOR:
            write_bytevector(p, v, flags);
            break;
        case SCM_TYPE_ENV:
            scm_puts(p, "#<environment>");
            break;
        case SCM_TYPE_PRIMITIVE:
            scm_printf(p, "#<primitive:%s>", c_str(primitive_name(v)));
            break;
        case SCM_TYPE_CLOSURE:
            if(is_undef(closure_name(v)))
                scm_printf(p, "#<closure:%p>", get_closure(v));
            else
                scm_printf(p, "#<closure:%s>", c_str(closure_name(v)));
            break;
        case SCM_TYPE_CONTINUATION:
            scm_puts(p, "#<continuation>");
            break;
        case SCM_TYPE_PARAMETER:
            scm_puts(p, "#<parameter:");
            write_(p, parameter_value(v), flags);
            scm_putc(p, '>');
            break;
        case SCM_TYPE_SYNTAX:
            scm_printf(p, "#<syntax:%s>", c_str(syntax_name(v)));
            break;
        case SCM_TYPE_ERROR:
            write_error(p, v, flags);
            break;
        case SCM_TYPE_PORT:
            scm_puts(p, "#<");
            if(is_binary_port(v))
                scm_puts(p, "binary-");
            scm_printf(p, "%sput-port:%s>", is_input_port(v) ? "in" : "out", port_name(v));
            break;
        case SCM_TYPE_LIBRARY:
            scm_puts(p, "#<library:");
            write_(p, library_name(v), flags);
            scm_puts(p, ">");
            break;
        case SCM_TYPE_RECORD:
            scm_puts(p, "#<record:");
            write_(p, record_type(v), flags);
            scm_puts(p, ">");
            break;
        case SCM_TYPE_FOREIGN_LIB:
            scm_printf(p, "#<foreign-lib:%s>", foreign_lib_name(v));
            break;
        case SCM_TYPE_FOREIGN_OBJ:
            scm_printf(p, "#<foreign-obj:%s>", foreign_obj_name(v));
            break;
        case SCM_TYPE_FOREIGN_PTR:
            scm_printf(p, "#<foreign-pointer:%p>", c_ptr(v));
            break;
        case SCM_TYPE_FOREIGN_TYPE:
            scm_printf(p, "#<foreign-type:%s>", foreign_type_name(v));
            break;
    }
}

static void write_char(scmval p, scmval v, short flags) {
    char c = c_char(v);
    scm_puts(p, "#\\");
    switch(c) {
        case '\a':  scm_puts(p, "alarm");       break;
        case '\b':  scm_puts(p, "backspace");   break;
        case '\n':  scm_puts(p, "newline");     break;
        case '\0':  scm_puts(p, "null");        break;
        case '\r':  scm_puts(p, "return");      break;
        case ' ':   scm_puts(p, "space");       break;
        case '\t':  scm_puts(p, "tab");         break;
        case 0xb:   scm_puts(p, "vtab");        break;
        default:    scm_putc(p, c);             break;
    }
}

static void write_pair(scmval p, scmval v, short flags) {
    if(flags & scm_mode_pp_quote) {
        if(!is_null(v) && is_eq(car(v), sym_quote)) {
            scm_putc(p, '\'');
            write_(p, cadr(v), flags);
            return;
        }
    }
    scm_putc(p, '(');
    for(scmval l = v; !is_null(l); l = cdr(l)) {
        write_(p, car(l), flags);
        if(!is_pair(cdr(l))) {
            scm_puts(p, " . ");
            write_(p, cdr(l), flags);
            break;
        }
        if(!is_null(cdr(l))) {
            scm_putc(p, ' ');
        }
    }
    scm_putc(p, ')');
}

static void write_vector(scmval p, scmval v, short flags) {
    scm_puts(p, "#(");
    for(int i = 0; i < vector_size(v); i++) {
        if(i > 0) {
            scm_putc(p, ' ');
        }
        write_(p, vector_ref(v, i), flags);
    }
    scm_putc(p, ')');
}

static void write_bytevector(scmval p, scmval v, short flags) {
    scm_puts(p, "#u8(");
    for(int i = 0; i < bytevector_size(v); i++) {
        if(i > 0) scm_putc(p, ' ');
        write_(p, bytevector_ref(v, i), flags);
    }
    scm_putc(p, ')');
}

static void write_error(scmval p, scmval v, short flags) {
    scm_puts(p, "[ERROR] ");
    if(is_eq(error_type(v), exn_error_type) || is_eq(error_type(v), cexn_error_type)) {
        scm_puts(p, "an error was raised with non-condition value ");
        write_(p, car(error_irritants(v)), flags);
    } else {
        write_(p, error_message(v), flags);
    }
}

static void write_values(scmval p, scmval v, short flags) {
    foreach(val, v) {
        write_(p, val, flags);
        scm_putc(p, '\n');
    }
}

