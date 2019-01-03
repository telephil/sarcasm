#pragma once
#define GC_DEBUG 1
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <setjmp.h>
#include <math.h>
#include <gc/gc.h>
#include <gc/cord.h>

#define IMPLEMENTATION_NAME     "sarcasm"
#define IMPLEMENTATION_VERSION  "0.1"

#if defined(__x86_64__)
    #define ARCH        "x86-64"
#elif defined(__i386__)
    #define ARCH        "i386"
#else
    #define ARCH        "unknown-arch"
#endif

#if defined(__linux__)
    #define OS  "linux"
#elif defined(__APPLE__)
    #define OS  "darwin"
#else
    #define OS  "unknown-platform"
#endif

// type aliases
typedef struct scmval scmval;
typedef uint8_t byte;
typedef int64_t fixnum;
typedef double flonum;

enum {
    SCM_TYPE_UNDEF,
    SCM_TYPE_VOID,
    SCM_TYPE_NULL,
    SCM_TYPE_EOF,
    SCM_TYPE_BOOL,
    SCM_TYPE_FIXNUM,
    SCM_TYPE_FLONUM,
    SCM_TYPE_CHAR,
    SCM_TYPE_STRING,
    SCM_TYPE_SYMBOL,
    SCM_TYPE_PAIR,
    SCM_TYPE_VECTOR,
    SCM_TYPE_BYTEVECTOR,
    SCM_TYPE_ENV,
    SCM_TYPE_SUBR,
    SCM_TYPE_CLOSURE,
    SCM_TYPE_SYNTAX,
    SCM_TYPE_ERROR,
    SCM_TYPE_PORT,
    SCM_TYPE_LIBRARY,
    SCM_TYPE_RECORD
};

struct scmval {
    int type;
    union {
        bool b;
        fixnum i;
        flonum d;
        char c;
        void* o;
    };
};

#define scm_new(T) GC_MALLOC(sizeof(T))
#define scm_new_array(S, T) GC_MALLOC(S*sizeof(T))
#define scm_new_atomic(S, T) GC_MALLOC_ATOMIC(S*sizeof(T))
#define scm_delete(P) GC_FREE(P)

static inline int type_of(scmval v) { return v.type; }
static inline scmval make_val(int type) { scmval v = { .type = type, .o = NULL }; return v; }
static inline scmval make_ptr(int type, void* o) { scmval v = { .type = type, .o = o }; return v; }

#include "scm/arity.h"
#include "scm/contract.h"
#include "scm/dict.h"
#include "scm/bool.h"
#include "scm/number.h"
#include "scm/char.h"
#include "scm/string.h"
#include "scm/symbol.h"
#include "scm/error.h"
#include "scm/pair.h"
#include "scm/vector.h"
#include "scm/bytevector.h"
#include "scm/port.h"
#include "scm/proc.h"
#include "scm/syntax.h"
#include "scm/env.h"
#include "scm/writer.h"
#include "scm/reader.h"
#include "scm/system.h"
#include "scm/library.h"
#include "scm/record.h"

// utilities
void init_eval(scmval);
scmval eval(scmval, scmval);

#define dbg(P,V) { printf(">>> " P ": '"); write(scm_current_output_port(), V, scm_mode_write); printf("'\n"); }
