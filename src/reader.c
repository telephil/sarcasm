#include <string.h>
#include "scm.h"

#define MAX_TOK_SIZE 1024

static scmval read_error_type;

// read helpers
static scmval read_aux(scmval, bool);
static scmval read_bool(scmval, char);
static scmval read_char(scmval);
static scmval read_string(scmval);
static scmval read_list(scmval);
static scmval read_vector(scmval);
static scmval read_bytevector(scmval);
static scmval read_any(scmval);
static char   read_hex_sequence(scmval);
static char skipws(scmval p);
static bool is_delimiter(char);
static bool is_initial(char);
static bool is_special_initial(char);
static bool is_peculiar_identifier(char);
static bool is_subsequent(char);
static bool is_special_subsequent(char);

void init_reader(scmval env) {
}

static void read_error(scmval p, const char* message, ...) {
    char *buf;
    va_list ap;
    va_start(ap, message);
    vasprintf(&buf, message, ap);
    va_end(ap);
    error(read_error_type, "%s (line: %d - pos: %d)", buf, port_line(p), port_pos(p));
}

scmval read(scmval p) {
    return read_aux(p, false);
}

scmval read_from_string(const char* s) {
    scmval p = open_input_string(s);
    scmval v = read(p);
    scm_close_input_port(p);
    return v;
}


static scmval read_aux(scmval p, bool in_list) {
    scmval v = scm_undef;
    char c;
    bool splicing = false;

start_read:
    c = skipws(p);
    if(c == EOF)
        return scm_eof;

    switch(c) {
        case ';':
            while(true) {
                c = scm_getc(p);
                if(c == EOF)
                    return scm_eof;
                if(c == '\n')
                    break;
            }
            goto start_read;
            break;
        case '(':
            v = read_list(p);
            break;
        case ')':
            if(!in_list) 
                read_error(p, "unexpected ')' character");
            v = sym_close_paren;
            break;
        case '.':
            if(scm_peek(p) == '.') {// ellipsis ?
                scm_getc(p);
                c = scm_getc(p);
                if(c != '.') read_error(p, "unexpected '%c' while reading ...");
                v = sym_ellipsis;
            } else if(isdigit(scm_peek(p))) { // flonum with no starting 0
                goto number_fallback;
            } else if(!in_list) {
                read_error(p, "unexpected '.' character");
            } else {
                v = sym_dot;
            }
            break;
        case '"':
            v = read_string(p);
            break;
        case '\'':
            v = read_aux(p, false);
            v = cons(sym_quote, cons(v, scm_null));
            break;
        case '`':
            v = read_aux(p, false);
            v = cons(sym_quasiquote, cons(v, scm_null));
            break;
        case ',':
            if(scm_peek(p) == '@') {
                scm_getc(p);
                splicing = true;
            }
            v = read_aux(p, false);
            v = cons(splicing ? sym_unquote_splicing : sym_unquote, cons(v, scm_null));
            break;
        case '#':
            c = scm_getc(p);
            switch(c) {
                case '|': 
                    {
                        int nest = 1;
                        while(true) {
                            c = scm_getc(p);
                            switch(c) {
                                case EOF:
                                    read_error(p, "unexpected end of file while reading nested comment");
                                    break;
                                case '|':
                                    if(scm_peek(p) == '#') {
                                        scm_getc(p); // eat #
                                        --nest;
                                        if(nest == 0) goto start_read;
                                    }
                                    break;
                                case '#':
                                    if(scm_peek(p) == '|') {
                                        scm_getc(p); // eat |
                                        ++nest;
                                    }
                                    break;
                            }
                        }
                    }
                case 't':
                case 'f':
                    v = read_bool(p, c);
                    break;
                case '\\':
                    v = read_char(p);
                    break;
                case '(':
                    v = read_vector(p);
                    break;
                case 'u':
                    c = scm_getc(p);
                    if(c != '8') read_error(p, "unexpected sequence '#u%c'", c);
                    c = scm_getc(p);
                    if(c != '(') read_error(p, "unexpected character '%c' while parsing bytevector", c);
                    v = read_bytevector(p);
                    break;
                case 'b':
                case 'o':
                case 'd':
                case 'x': // number radix
                    scm_ungetc(p, c);
                    goto number_fallback;
                default:
                    read_error(p, "unexpected character '%c' after #", c);
            }
            break;
        default:
number_fallback:
            scm_ungetc(p, c);
            v = read_any(p);
            break;
    }
    return v;
}

static scmval read_bool(scmval p, char initial) {
    char buf[6];
    buf[0] = initial;
    int size = 1;
    for(int i = 1; i < 5; i++) {
        char c = scm_getc(p);
        if(c == EOF || is_delimiter(c)) {
            scm_ungetc(p, c);
            break;
        }
        buf[size++] = c;
    }
    buf[size] = '\0';
    scmval b = scm_false;
    if(size == 1)
        b = initial == 't' ? scm_true : scm_false;
    else if(strcmp(buf, "true") == 0)
        b = scm_true;
    else if(strcmp(buf, "false") == 0)
        b = scm_false;
    else
        read_error(p, "unexpected token '#%s' while reading boolean", buf);
    return b;
}

static scmval read_any(scmval p) {
    scmval v = scm_undef;
    char buf[MAX_TOK_SIZE], c;
    int size = 0;
    while(true) {
        c = scm_getc(p);
        if(c == EOF || is_delimiter(c)) {
            scm_ungetc(p, c);
            break;
        }
        buf[size++] = c;
        if(size > (MAX_TOK_SIZE - 1))
            read_error(p, "token too long");
    }
    buf[size] = '\0';
    // Try to read number
    v = string_to_number(buf);
    if(!is_false(v)) {
        return v;
    }
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
            v = intern(buf);
    } else if(is_peculiar_identifier(buf[0])) {
        v = intern(buf);
    } else {
        read_error(p, "unrecognized token '%s'", buf);
    }
    return v;
}

static scmval read_char(scmval p) {
    scmval v;
    char buf[MAX_TOK_SIZE], c;
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
            read_error(p, "token too long while reading char");
    }
    buf[i] = '\0';
    c = buf[0];
    if(i == 1) {
        v = s_char(c);
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
            read_error(p, "invalid character name '%s'", buf);
        v = s_char(c);
    }
    return v;
}

static scmval read_string(scmval p) {
    scmval v;
    char c, *buf;
    int len = MAX_TOK_SIZE, i = 0;
    buf = GC_MALLOC(len*sizeof(char));
    while(true) {
        c = scm_getc(p);
        if(c == EOF) {
            read_error(p, "unexpected end of file while reading string");
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
                case 'x': c = read_hex_sequence(p); break;
                default:
                   read_error(p, "invalid escaped character '%c' in string", c);
            }
        }
        if((i+1) >= len) {
            len = 1.5 * len;
            buf = GC_REALLOC(buf, len*sizeof(char));
        }
        buf[i++] = c;
    }
    buf[i] = '\0';
    v = s_str(buf);
    return v;
}

static scmval read_list(scmval p) {
    scmval h, t;
    scmval v;
    h = t = scm_null;
    v = read_aux(p, true);
    while(!is_eq(v, sym_close_paren)) {
        if(is_eof(v))
            read_error(p, "unexpected end of file while reading list");
        if(is_eq(v, sym_dot)) {
            v = read_aux(p, true);
            if(is_eof(v))
                read_error(p, "unexpected end of file after . in list");
            if(is_eq(v, sym_close_paren))
                read_error(p, "unexpected ) after . in list");
            if(is_null(h))
                read_error(p, "unexpected . in empty list");
            setcdr(t, v);
            v = read_aux(p, true);
            if(is_eof(v))
                read_error(p, "unexpected end of file in dotted list - expected )");
            if(!is_eq(v, sym_close_paren))
                read_error(p, "unexpected value in dotted list - expected )");
            return h;
        }
        scmval c = cons(v, scm_null);
        if(is_null(h)) {
            h = t = c;
        } else {
            setcdr(t, c);
            t = c;
        }            
        v = read_aux(p, true);
    }
    return h;
}

static scmval read_vector(scmval p) {
    scmval h, t;
    scmval v;
    int size = 0;
    h = t = scm_null;
    v = read_aux(p, true);
    while(!is_eq(v, sym_close_paren)) {
        if(is_eof(v))
            read_error(p, "unexpected end of file while reading vector");
        scmval c = cons(v, scm_null);
        if(is_null(h)) {
            h = t = c;
        } else {
            setcdr(t, c);
            t = c;
        }
        ++size;
        v = read_aux(p, true);
    }
    v = make_vector_from_list(size, h);
    return v;
}

static scmval read_bytevector(scmval p) {
    scmval h, t;
    scmval v;
    int size = 0;
    h = t = scm_null;
    v = read_aux(p, true);
    while(!is_eq(v, sym_close_paren)) {
        if(is_eof(v))
            read_error(p, "unexpected end of file while reading bytevector");
        if(!is_byte(v))
            read_error(p, "invalid byte %s found while reading bytevector", scm_to_cstr(v));
        scmval c = cons(v, scm_null);
        if(is_null(h)) {
            h = t = c;
        } else {
            setcdr(t, c);
            t = c;
        }
        ++size;
        v = read_aux(p, true);
    }
    v = make_bytevector_from_list(size, h);
    return v;
}

static char read_hex_sequence(scmval p) {
    char buffer[16];
    int i = 0;
    while(true) {
        char c  = scm_getc(p);
        if(c == EOF)
            read_error(p, "unexpected end of file while reading hex sequence");
        if(c == ';')
            break;
        if(i == sizeof(buffer) - 1)
            read_error(p, "hex sequence too long");
        buffer[i++] = c;
    }
    buffer[i] = '\0';
    fixnum n = strtol(buffer, NULL, 16);
//    if(errno == ERANGE)
//        read_error(p, "unable to convert hex sequence to a valid character");
    return (char)n;
}

static char skipws(scmval p) {
    char c;
    while(true) {
        c = scm_getc(p);
        if(c == EOF)
            return EOF;
        if(!isspace(c))
            return c;
    }
}

static bool is_delimiter(char c) {
    return isspace(c)
        || c == '('
        || c == ')'
        || c == ';'
        || c == '"'
        || c == 0;
}

static bool is_initial(char c) {
    return isalpha(c)
        || is_special_initial(c);
}

static bool is_special_initial(char c)
{
  return (c == '!' || c == '$' || c == '%' || c == '&' ||
          c == '*' || c == '/' || c == ':' || c == '<' ||
          c == '=' || c == '>' || c == '?' || c == '^' ||
          c == '_');
}

static bool is_peculiar_identifier(char c)
{
    return c == '+' || c == '-';
}

static bool is_subsequent(char c) 
{
  return (is_initial(c) || isdigit(c) || is_special_subsequent(c));
}

static bool is_special_subsequent(char c)
{
  return (c == '+' || c == '-' || c == '.' || c == '@');
}

