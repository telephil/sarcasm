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
#include <gmp.h>

////////////////////////////////////////////////////////////////////////////////
// SCMVAL DEFINITION
////////////////////////////////////////////////////////////////////////////////
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
    SCM_TYPE_BIGNUM,
    SCM_TYPE_FLONUM,
    SCM_TYPE_CHAR,
    SCM_TYPE_STRING,
    SCM_TYPE_SYMBOL,
    SCM_TYPE_PAIR,
    SCM_TYPE_VECTOR,
    SCM_TYPE_BYTEVECTOR,
    SCM_TYPE_ENV,
    SCM_TYPE_PRIMITIVE,
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

static inline int type_of(scmval v) { return v.type; }
static inline scmval make_val(int type) { scmval v = { .type = type, .o = NULL }; return v; }
static inline scmval make_ptr(int type, void* o) { scmval v = { .type = type, .o = o }; return v; }

////////////////////////////////////////////////////////////////////////////////
// GLOBALS
////////////////////////////////////////////////////////////////////////////////
extern scmval scm_undef;
extern scmval scm_void;
extern scmval scm_null;
extern scmval scm_true;
extern scmval scm_false;
extern scmval scm_eof;

extern scmval scm_g_command_line;
extern scmval scm_g_features;
////////////////////////////////////////////////////////////////////////////////
// TYPES
////////////////////////////////////////////////////////////////////////////////
#include "config.h"
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
#include "scm/control.h"
#include "scm/library.h"
#include "scm/record.h"
#include "scm/eval.h"

////////////////////////////////////////////////////////////////////////////////
// GC FUNCTIONS
////////////////////////////////////////////////////////////////////////////////
#define scm_new(T)              GC_MALLOC(sizeof(T))
#define scm_new_array(S, T)     GC_MALLOC(S*sizeof(T))
#define scm_new_atomic(S, T)    GC_MALLOC_ATOMIC(S*sizeof(T))
#define scm_delete(P)           GC_FREE(P)
static inline void* scm_gc_malloc(size_t size) { return GC_MALLOC(size); }
static inline void* scm_gc_realloc(void* ptr, size_t old_size, size_t new_size) { return GC_REALLOC(ptr, new_size); }
static inline void  scm_gc_free(void* ptr, size_t size) { GC_FREE(ptr); }
static inline void  scm_gc_collect() { GC_gcollect(); }

////////////////////////////////////////////////////////////////////////////////
// BOOT
////////////////////////////////////////////////////////////////////////////////
void scm_boot(int, char*[]);

////////////////////////////////////////////////////////////////////////////////
// HELPER MACROS
////////////////////////////////////////////////////////////////////////////////
#define dbg(P,V) { printf(">>> " P ": '"); write(scm_current_output_port(), V, scm_mode_write); printf("'\n"); }

#define opt_arg(ARG,OPT) ARG = is_undef(ARG) ? OPT : ARG
#define check_arg(N,C,V) if(!C.pred(V)) (type_error(N,C,V))
#define check_args(N,C,AC,AV) for(int i = 0; i < AC; i++) { if(!C.pred(AV[i])) (type_error(N,C,AV[i])); }
#define check_range(N,V,L,H) if((V) < (L) || (V) >= (H)) range_error(N, (V), (L), (H))

