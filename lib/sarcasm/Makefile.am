sarcasmdir = $(scmlibdir)/sarcasm
dist_sarcasm_DATA = init.scm foreign.scm process.scm readline.scm repl.scm string.scm syntax.scm test.scm

# C modules
pkglib_LTLIBRARIES = libsarcasm_process.la libsarcasm_readline.la
pkglibdir = $(libdir)

libsarcasm_process_la_SOURCES = process.c
libsarcasm_process_la_CFLAGS  = -I$(top_srcdir)/include -Wall -Werror -pedantic -std=c11 $(GC_CFLAGS) $(FFI_CFLAGS) $(GMP_CFLAGS) -DSCM_LIBRARY_PATH='"$(scmlibdir)"'
libsarcasm_process_la_LDFLAGS = -avoid-version -module -shared -export-dynamic

libsarcasm_readline_la_SOURCES = readline.c
libsarcasm_readline_la_CFLAGS  = -I$(top_srcdir)/include -Wall -Werror -pedantic -std=c11 $(GC_CFLAGS) $(FFI_CFLAGS) $(GMP_CFLAGS) $(READLINE_CFLAGS) -DSCM_LIBRARY_PATH='"$(scmlibdir)"'
libsarcasm_readline_la_LDFLAGS = -avoid-version -module -shared -export-dynamic
