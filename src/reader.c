#include <string.h>
#include "scm.h"

#define MAX_TOK_SIZE 1024

static scmval read_error_type;
static scmval scm_close_paren;
static scmval scm_dot;
static scmval scm_quote;
static scmval scm_quasiquote;
static scmval scm_unquote;
static scmval scm_unquote_splicing;

// read helpers
static scmval read_aux(scm_ctx_t*, scmval, bool);
static scmval read_char(scm_ctx_t*, scmval);
static scmval read_string(scm_ctx_t*, scmval);
static scmval read_list(scm_ctx_t*, scmval);
static scmval read_vector(scm_ctx_t*, scmval);
static scmval read_any(scm_ctx_t*, scmval);
static scm_char_t skipws(scmval p);
static bool is_delimiter(scm_char_t);
static bool is_initial(scm_char_t);
static bool is_special_initial(scm_char_t);
static bool is_peculiar_identifier(scm_char_t);
static bool is_subsequent(scm_char_t);
static bool is_special_subsequent(scm_char_t);
static bool is_valid_digit(scm_char_t, int);


void init_reader(scm_ctx_t* ctx) {
    scm_close_paren = intern(ctx, make_symbol(")"));
    scm_dot         = intern(ctx, make_symbol("."));
    read_error_type = intern(ctx, make_symbol("read-error"));
    scm_quote       = intern(ctx, make_symbol("quote"));
    scm_quasiquote  = intern(ctx, make_symbol("quasiquote"));
    scm_unquote     = intern(ctx, make_symbol("unquote"));
    scm_unquote_splicing = intern(ctx, make_symbol("unquote-splicing"));
}

void read_error(scm_ctx_t* ctx, scmval p, const char* message, ...) {
    char *buf;
    scmval e;
    va_list ap;
    va_start(ap, message);
    vasprintf(&buf, message, ap);
    va_end(ap);
    e = error(read_error_type, "%s (line: %d - pos: %d)", buf, port_line(p), port_pos(p));
    throw(ctx, e);
}

scmval read(scm_ctx_t* ctx, scmval p) {
    return read_aux(ctx, p, false);
}

static scmval read_aux(scm_ctx_t* ctx, scmval p, bool in_list) {
    scmval v = scm_undef;
    scm_char_t c;

    c = skipws(p);
    if(c == EOF)
        return scm_eof;

    switch(c) {
        case '(':
            v = read_list(ctx, p);
            break;
        case ')':
            if(!in_list) 
                read_error(ctx, p, "unexpected ')' character");
            v = scm_close_paren;
            break;
        case '.':
            if(!in_list)
                read_error(ctx, p, "unexpected '.' character");
            v = scm_dot;
            break;
        case '"':
            v = read_string(ctx, p);
            break;
        case '#':
            c = scm_getc(p);
            switch(c) {
                case 't':
                    v = scm_true;
                    break;
                case 'f':
                    v = scm_false;
                    break;
                case '\\':
                    v = read_char(ctx, p);
                    break;
                case '(':
                    v = read_vector(ctx, p);
                    break;
                case 'b':
                case 'o':
                case 'd':
                case 'x': // number radix
                    scm_ungetc(p, c);
                    goto number_fallback;
                default:
                    read_error(ctx, p, "unexpected character '%c' after #", c);
            }
            break;
        default:
number_fallback:
            scm_ungetc(p, c);
            v = read_any(ctx, p);
            break;
    }
    return v;
}

static scmval read_number(scm_char_t* buf) {
    int base = 10;
    bool dot = false;
    bool neg = false;
    bool is_int = true;
    scm_char_t *p = buf, *q;

    // base
    if(*p == '#') {
        p++;
        switch(*p) {
            case 'b': base =  2; break;
            case 'o': base =  8; break;
            case 'd': base = 10; break;
            case 'x': base = 16; break;
            default: 
              return scm_undef;
        }
        p++;
    } 
    if(*p == '+' || *p == '-') {
        if(*p == '-')
            neg = true;
        p++;
    }
    for(q = p; *q; q++) {
        if(*q == '.') {
            if(dot) return scm_undef; // already found a dot
            if(base != 10) return scm_undef;
            dot = true;
            is_int = false;
        } else if(!is_valid_digit(*q, base))
            return scm_undef;
    }

    if(is_int) {
        scm_fixnum_t l = strtol(p, NULL, base);
        if(neg) l = -l;
        return make_fixnum(l);
    } else {
        scm_flonum_t f = strtod(p, NULL);
        if(neg) f = -f;
        return make_flonum(f);
    }

    return scm_undef;
}

static scmval read_any(scm_ctx_t* ctx, scmval p) {
    scmval v = scm_undef;
    scm_char_t buf[MAX_TOK_SIZE], c;
    int size = 0;
    while(true) {
        c = scm_getc(p);
        if(c == EOF || is_delimiter(c)) {
            scm_ungetc(p, c);
            break;
        }
        buf[size++] = c;
        if(size > (MAX_TOK_SIZE - 1))
            read_error(ctx, p, "token too long");
    }
    buf[size] = '\0';
    // Try to read number
    v = read_number(buf);
    if(!is_undef(v))
        return v;
    // Check if identifier
    if(is_initial(buf[0])) {
        bool is_identifier = true;
        for(int i = 1; i < size; i++) {
            if(!is_subsequent(buf[i])) {
                is_identifier = false;
                break;
            }
        }
        if(is_identifier)
            v = intern(ctx, make_symbol(buf));
    } else if(is_peculiar_identifier(buf[0])) {
        v = intern(ctx, make_symbol(buf));
    } else {
        read_error(ctx, p, "unrecognized token '%s'", buf);
    }
    return v;
}

static scmval read_char(scm_ctx_t* ctx, scmval p) {
    scmval v;
    scm_char_t buf[MAX_TOK_SIZE], c;
    int i = 0;
    while(true) {
        c = scm_getc(p);
        if(c == EOF || isspace(c))
            break;
        if(is_delimiter(c)) {
            scm_ungetc(p, c);
            break;
        }
        buf[i++] = c;
        if(i > (MAX_TOK_SIZE - 1))
            read_error(ctx, p, "token too long while reading char");
    }
    buf[i] = '\0';
    c = buf[0];
    if(i == 1) {
        v = make_char(c);
    } else {
        if(c == 'a' && !strncmp(buf, "alarm", i))
            c = '\a';
        else if(c == 'b' && !strncmp(buf, "backspace", i))
            c = '\b';
        else if(c == 'n' && !strncmp(buf, "newline", i))
            c = '\n';
        else if(c == 'n' && !strncmp(buf, "null", i))
            c = '\0';
        else if(c == 'r' && !strncmp(buf, "return", i))
            c = '\r';
        else if(c == 's' && !strncmp(buf, "space", i))
            c = ' ';
        else if(c == 't' && !strncmp(buf, "tab", i))
            c = '\t';
        else if(c == 'v' && !strncmp(buf, "vtab", i))
            c = 0xb;
        else
            read_error(ctx, p, "invalid character name '%s'", buf);
        v = make_char(c);
    }
    return v;
}

static scmval read_string(scm_ctx_t* ctx, scmval p) {
    scmval v;
    scm_char_t c, *buf;
    int len = MAX_TOK_SIZE, i = 0;
    buf = GC_MALLOC(len*sizeof(scm_char_t));
    while(true) {
        c = scm_getc(p);
        if(c == EOF) {
            read_error(ctx, p, "unexpected end of file while reading string");
        } else if(c == '"') {
            break;
        } else if(c == '\\') {
            c = scm_getc(p);
            switch(c) {
                case 'a': c = '\a'; break;
                case 'b': c = '\b'; break;
                case 't': c = '\t'; break;
                case 'n': c = '\n'; break;
                case 'r': c = '\r'; break;
                case '"': c = '"';  break;
                case '\\': c = '\\'; break;
                default:
                   read_error(ctx, p, "invalid escaped character '%c' in string", c);
            }
        }
        if((i+1) >= len) {
            len = 1.5 * len;
            buf = GC_REALLOC(buf, len*sizeof(char));
        }
        buf[i++] = c;
    }
    buf[i] = '\0';
    v = make_string(buf);
    return v;
}

static scmval read_list(scm_ctx_t* ctx, scmval p) {
    scmval h, t;
    scmval v;
    h = t = scm_null;
    v = read_aux(ctx, p, true);
    while(!is_eq(v, scm_close_paren)) {
        if(is_eof(v))
            read_error(ctx, p, "unexpected end of file while reading list");
        if(is_eq(v, scm_dot)) {
            v = read_aux(ctx, p, true);
            if(is_eof(v))
                read_error(ctx, p, "unexpected end of file after . in list");
            if(is_eq(v, scm_close_paren))
                read_error(ctx, p, "unexpected ) after . in list");
            if(is_null(h))
                read_error(ctx, p, "unexpected . in empty list");
            setcdr(t, v);
            v = read_aux(ctx, p, true);
            if(is_eof(v))
                read_error(ctx, p, "unexpected end of file in dotted list - expected )");
            if(!is_eq(v, scm_close_paren))
                read_error(ctx, p, "unexpected value in dotted list - expected )");
            return h;
        }
        scmval c = cons(v, scm_null);
        if(is_null(h)) {
            h = t = c;
        } else {
            setcdr(t, c);
            t = c;
        }            
        v = read_aux(ctx, p, true);
    }
    return h;
}

static scmval read_vector(scm_ctx_t* ctx, scmval p) {
    scmval h, t;
    scmval v;
    int size = 0;
    h = t = scm_null;
    v = read_aux(ctx, p, true);
    while(!is_eq(v, scm_close_paren)) {
        if(is_eof(v))
            read_error(ctx, p, "unexpected end of file while reading vector");
        scmval c = cons(v, scm_null);
        if(is_null(h)) {
            h = t = c;
        } else {
            setcdr(t, c);
            t = c;
        }
        ++size;
        v = read_aux(ctx, p, true);
    }
    v = make_vector_from_list(size, h);
    return v;
}

static scm_char_t skipws(scmval p) {
    scm_char_t c;
    while(true) {
        c = scm_getc(p);
        if(c == EOF)
            return EOF;
        if(!isspace(c))
            return c;
    }
}

static bool is_delimiter(scm_char_t c) {
    return isspace(c)
        || c == '('
        || c == ')'
        || c == ';'
        || c == '"'
        || c == 0;
}

static bool is_initial(scm_char_t c) {
    return isalpha(c)
        || is_special_initial(c);
}

static bool is_special_initial(scm_char_t c)
{
  return (c == '!' || c == '$' || c == '%' || c == '&' ||
          c == '*' || c == '/' || c == ':' || c == '<' ||
          c == '=' || c == '>' || c == '?' || c == '^' ||
          c == '_');
}

static bool is_peculiar_identifier(scm_char_t c)
{
    return c == '+' || c == '-';
}

static bool is_subsequent(scm_char_t c) 
{
  return (is_initial(c) || isdigit(c) || is_special_subsequent(c));
}

static bool is_special_subsequent(scm_char_t c)
{
  return (c == '+' || c == '-' || c == '.' || c == '@');
}

static bool is_valid_digit(scm_char_t c, int base) {
    bool valid = false;
    switch(base) {
        case 2:
            valid = (c == '0' || c == '1');
            break;
        case 8:
            valid = (c >= '0' && c <= '7');
            break;
        case 10:
            valid = (c >= '0' && c <= '9');
            break;
        case 16:
            valid = (c >= '0' && c <= '9') 
                 || (c >= 'a' && c <= 'f')
                 || (c >= 'A' && c <= 'F');
            break;
    }
    return valid;
}
