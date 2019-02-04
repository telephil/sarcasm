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
#include <unistd.h>
#include <sys/errno.h>
#include <gc/gc.h>
#include <gmp.h>

////////////////////////////////////////////////////////////////////////////////
// SCMVAL DEFINITION
////////////////////////////////////////////////////////////////////////////////
typedef struct scmval scmval;
typedef unsigned short scm_type_t;
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
    SCM_TYPE_CONTINUATION,
    SCM_TYPE_PARAMETER,
    SCM_TYPE_SYNTAX,
    SCM_TYPE_ERROR,
    SCM_TYPE_PORT,
    SCM_TYPE_LIBRARY,
    SCM_TYPE_RECORD,
};

enum {
    scm_flag_immutable  = 1<<0,
    scm_flag_values     = 1<<1,
};

struct scmval {
    scm_type_t type;
    short flags;
    union {
        bool b;
        fixnum i;
        flonum d;
        char c;
        void* o;
    };
};

static inline scm_type_t type_of(scmval v) { return v.type; }
static inline scmval make_val(scm_type_t type) { scmval v = { .type = type, .flags = 0, .o = NULL }; return v; }
static inline scmval make_ptr(scm_type_t type, void* o) { scmval v = { .type = type, .flags = 0, .o = o }; return v; }

#define copy_flags(from, to) to.flags = from.flags
#define set_flag(v, flag) v.flags |= flag
#define set_immutable(v) set_flag(v, scm_flag_immutable)
static inline bool   has_flag(scmval v, int flag) { return v.flags & flag; }
static inline bool is_immutable(scmval v) { return has_flag(v, scm_flag_immutable); }

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
typedef void(*scm_printer)(scmval,scmval,short);
scm_type_t register_type(const char*);
const char* get_type_name(scm_type_t);
void register_type_printer(scm_type_t, scm_printer);
scm_printer get_type_printer(scm_type_t);
scm_type_t register_type_with_printer(const char*, scm_printer);

////////////////////////////////////////////////////////////////////////////////
// CORE
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
#include "scm/vector.h"
#include "scm/bytevector.h"
#include "scm/port.h"
#include "scm/pair.h"
#include "scm/proc.h"
#include "scm/syntax.h"
#include "scm/env.h"
#include "scm/writer.h"
#include "scm/reader.h"
#include "scm/system.h"
#include "scm/control.h"
#include "scm/parameter.h"
#include "scm/library.h"
#include "scm/record.h"
#include "scm/foreign.h"
#include "scm/eval.h"

////////////////////////////////////////////////////////////////////////////////
// GC FUNCTIONS
////////////////////////////////////////////////////////////////////////////////
static inline void* scm_gc_malloc(size_t size) { return GC_MALLOC(size); }
static inline void* scm_gc_malloc_atomic(size_t size) { return GC_MALLOC_ATOMIC(size); }
static inline void* scm_gc_realloc(void* ptr, size_t new_size) { return GC_REALLOC(ptr, new_size); }
static inline void* scm_gc_realloc_z(void* ptr, size_t old, size_t news) { return scm_gc_realloc(ptr, news); }
static inline void  scm_gc_free(void* ptr) { GC_FREE(ptr); }
static inline void  scm_gc_free_z(void* ptr, size_t s) { scm_gc_free(ptr); }
static inline void  scm_gc_collect() { GC_gcollect(); }
static inline char* scm_gc_strdup(const char* s) { return GC_STRDUP(s); }

////////////////////////////////////////////////////////////////////////////////
// BOOT
////////////////////////////////////////////////////////////////////////////////
void scm_boot(int, char*[]);

////////////////////////////////////////////////////////////////////////////////
// HELPER MACROS
////////////////////////////////////////////////////////////////////////////////
#define dbg(P,V) { printf(">>> " P ": '"); scm_write(V, scm_current_output_port()); printf("'\n"); }

#define opt_arg(ARG,OPT) ARG = is_undef(ARG) ? OPT : ARG
#define check_arg(N,C,V) if(!C.pred(V)) (type_error(N,C,V))
#define check_args(N,C,AC,AV) for(int i = 0; i < AC; i++) { if(!C.pred(AV[i])) (type_error(N,C,AV[i])); }
#define check_range(N,V,L,H) if((V) < (L) || (V) >= (H)) range_error(N, (V), (L), (H))
#define check_mutable(N,T,V) if(is_immutable((V))) mutability_error(N, T)

