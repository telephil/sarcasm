bin_PROGRAMS = sarcasm

sarcasm_CFLAGS = -I$(top_srcdir)/include -Wall -Werror -pedantic -std=c11 $(GC_CFLAGS) $(FFI_CFLAGS) $(GMP_CFLAGS) $(READLINE_CFLAGS) -DSCM_LIBRARY_PATH='"$(scmlibdir)"'
sarcasm_LDADD = ../libsarcasm/libsarcasm.la
sarcasm_SOURCES = main.c
