PACKAGE_NAME	= sarcasm
PACKAGE_VERSION	= 0.1

################################################################################
PREFIX		= /usr/local
BINDIR		= $(PREFIX)/bin
LIBDIR		= $(PREFIX)/lib
DATADIR		= $(PREFIX)/share/$(PACKAGE_NAME)
SCMLIBDIR	= $(DATADIR)/lib

################################################################################
CC		= clang
CD		= cd
RM		= rm -f
INSTALL	= install
MKDIR	= $(INSTALL) -d
RMDIR	= rmdir

################################################################################
BASE_CFLAGS 		= -g -std=c11 -Wall -Werror -pedantic -Isrc
BASE_LDFLAGS		=
LIB_LDFLAGS			= -fPIC -shared

GC_CFLAGS			= $(shell pkg-config --cflags bdw-gc)
GC_LDFLAGS			= $(shell pkg-config --libs bdw-gc) -lcord
GMP_CFLAGS			= 
GMP_LDFLAGS			= -lgmp
FFI_CFLAGS			= $(shell pkg-config --cflags libffi)
FFI_LDFLAGS			= $(shell pkg-config --libs libffi)
READLINE_CFLAGS 	=
READLINE_LDFLAGS 	= -lreadline
LIBSARCASM_CFLAGS	=
LIBSARCASM_LDFLAGS	= -L. -lsarcasm

################################################################################
INCLUDES 		= src/scm.h src/scm/symbols.inc $(wildward src/scm/*.h)
SARCASM_OBJS	= src/main.o
LIBSARCASM_OBJS = src/scm.o	\
				  src/bool.o \
				  src/bytevector.o \
				  src/char.o \
				  src/control.o \
				  src/dict.o \
				  src/env.o \
				  src/error.o \
				  src/eval.o \
				  src/foreign.o \
				  src/library.o \
				  src/number.o \
				  src/pair.o \
				  src/parameter.o \
				  src/port.o \
				  src/proc.o \
				  src/reader.o \
				  src/record.o \
				  src/string.o \
				  src/symbol.o \
				  src/syntax.o \
				  src/system.o \
				  src/vector.o \
				  src/writer.o
MODPROCESS_OBJS	= lib/sarcasm/process.o
MODRL_OBJS		= lib/sarcasm/readline.o

################################################################################
SARCASM		= scm
LIBSARCASM	= libsarcasm.dylib
MODPROCESS	= libsarcasm_process.dylib
MODRL		= libsarcasm_readline.dylib
ALL_MODULES	= $(MODPROCESS) $(MODRL)

################################################################################
all:	$(LIBSARCASM) $(SARCASM) $(ALL_MODULES)

%.o:	%.c $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(GC_CFLAGS) $(GMP_CFLAGS) $(FFI_CFLAGS) -o $@ $<

$(SARCASM_OBJS):	$(SARCASM_OBJS:.o=.c) $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(FFI_CFLAGS) -o $@ $<

$(MODPROCESS_OBJS):	 $(MODPROCESS_OBJS:.o=.c) $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(FFI_CFLAGS) -o $@ $<

$(MODRL_OBJS):	$(MODRL_OBJS:.o=.c) $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(FFI_CFLAGS) -o $@ $<

$(LIBSARCASM):	$(LIBSARCASM_OBJS)
	$(CC) $(BASE_LDFLAGS) $(GC_LDFLAGS) $(GMP_LDFLAGS) $(FFI_LDFLAGS) $(LIB_LDFLAGS) -o $@ $^

$(MODPROCESS):	$(MODPROCESS_OBJS)
	$(CC) $(BASE_LDFLAGS) $(GC_LDFLAGS) $(LIBSARCASM_LDFLAGS) $(LIB_LDFLAGS) -o $@ $^

$(MODRL):	$(MODRL_OBJS)
	$(CC) $(BASE_LDFLAGS) $(GC_LDFLAGS) $(READLINE_LDFLAGS) $(LIBSARCASM_LDFLAGS) $(LIB_LDFLAGS) -o $@ $^

$(SARCASM):	$(SARCASM_OBJS) $(LIBSARCASM)
	$(CC) $(BASE_LDFLAGS) $(LIBSARCASM_LDFLAGS) -o $@ $<

test:	$(SARCASM)
	@./$(SARCASM) -l ./tests/r7rs.scm

install: all
	$(MKDIR) $(BINDIR)
	$(INSTALL) $(SARCASM) $(BINDIR)/
	$(MKDIR) $(LIBDIR)
	$(INSTALL) $(LIBSARCASM) $(LIBDIR)/
	$(INSTALL) $(ALL_MODULES) $(LIBDIR)/
	$(MKDIR) $(SCMLIBDIR) $(SCMLIBDIR)/sarcasm $(SCMLIBDIR)/scheme
	$(INSTALL) -m 644 lib/sarcasm/*.scm $(SCMLIBDIR)/sarcasm/
	$(INSTALL) -m 644 lib/scheme/*.scm $(SCMLIBDIR)/scheme/

uninstall:
	-$(RM) $(BINDIR)/$(SARCASM)
	-$(RM) $(LIBDIR)/$(LIBSARCASM)
	-$(CD) $(LIBDIR) && $(RM) $(ALL_MODULES)
	-$(RM) $(LIBDIR)/$(MODPROCESS)
	-$(RM) $(LIBDIR)/$(MODRL)
	-$(RM) $(SCMLIBDIR)/sarcasm/*.scm
	-$(RMDIR) $(SCMLIBDIR)/sarcasm
	-$(RM) $(SCMLIBDIR)/scheme/*.scm
	-$(RMDIR) $(SCMLIBDIR)/scheme
	-$(RMDIR) $(SCMLIBDIR)
	-$(RMDIR) $(DATADIR)

.PHONY:	clean 
clean:
	-$(RM) $(SARCASM) $(SARCASM_OBJS) $(LIBSARCASM) $(LIBSARCASM_OBJS) $(MODPROCESS_OBJS) $(MODPROCESS) $(MODRL_OBJS) $(MODRL)

