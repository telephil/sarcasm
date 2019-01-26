lib_LTLIBRARIES = libsarcasm.la

libsarcasm_la_CFLAGS = -I$(top_srcdir)/include -Wall -Werror -pedantic -std=c11 $(GC_CFLAGS) $(FFI_CFLAGS) $(GMP_CFLAGS) $(READLINE_CFLAGS) -DSCM_LIBRARY_PATH='"$(scmlibdir)"'
libsarcasm_la_LIBADD = $(GC_LIBS) $(FFI_LIBS) $(GMP_LIBS) $(READLINE_LIBS) -lcord

libsarcasm_la_SOURCES = \
    dict.c		\
    eval.c		\
    char.c		\
    writer.c	\
    pair.c		\
    config.h	\
    vector.c	\
    scm.c		\
    number.c	\
    system.c	\
    string.c	\
    foreign.c	\
    control.c	\
    khash.h		\
    reader.c	\
    env.c	\
    error.c	\
    library.c	\
    parameter.c	\
    symbol.c	\
    bytevector.c	\
    proc.c	\
    port.c	\
    syntax.c	\
    record.c	\
    scm.h	\
    scm/error.h	\
    scm/env.h	\
    scm/reader.h	\
    scm/control.h	\
    scm/port.h	\
    scm/syntax.h	\
    scm/proc.h	\
    scm/bytevector.h	\
    scm/symbol.h	\
    scm/library.h	\
    scm/parameter.h	\
    scm/bool.h		\
    scm/record.h	\
    scm/eval.h		\
    scm/dict.h		\
    scm/pair.h		\
    scm/char.h		\
    scm/writer.h	\
    scm/contract.h	\
    scm/system.h	\
    scm/number.h	\
    scm/vector.h	\
    scm/arity.h		\
    scm/foreign.h	\
    scm/string.h	\
    bool.c
