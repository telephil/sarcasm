name	= sarcasm
version	= 0.1

################################################################################
# Directories
prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
includedir = $(exec_prefix)/include
datarootdir = $(prefix)/share
datadir = $(datarootdir)
scmlibdir = $(datadir)/$(name)/lib

################################################################################
# Shell commands
SHELL = sh
CD = cd
RM = rm -f
INSTALL	= install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644
MKDIR = $(INSTALL) -d
RMDIR = rmdir
CC = clang

################################################################################
# Compilation variables
CFLAGS = -g
ifeq ($(PLATFORM),linux)
	ALL_CFLAGS = -DSARCASM_LINUX -D_GNU_SOURCE -fPIC -std=c11 -Wall -Werror -pedantic -Isrc $(CFLAGS)
	ALL_LDFLAGS = -lm
else
	ALL_CFLAGS = -fPIC -std=c11 -Wall -Werror -pedantic -Isrc $(CFLAGS)
	ALL_LDFLAGS = 
endif
LIB_LDFLAGS = -fPIC -shared
CONFIG_CFLAGS = -DNAME='"$(name)"' -DVERSION='"$(version)"' -DSCMLIBDIR='"$(scmlibdir)"'
GC_CFLAGS = $(shell pkg-config --cflags bdw-gc)
GC_LDFLAGS = $(shell pkg-config --libs bdw-gc)
GMP_CFLAGS = 
GMP_LDFLAGS = -lgmp
FFI_CFLAGS = $(shell pkg-config --cflags libffi)
FFI_LDFLAGS = $(shell pkg-config --libs libffi)
READLINE_CFLAGS =
READLINE_LDFLAGS = -lreadline


################################################################################
# Sources
includes 		= src/scm.h src/scm/symbols.inc $(wildward src/scm/*.h)
sarcasm_objs	= src/main.o
libsarcasm_objs = src/scm.o	\
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
libprocess_objs	= lib/sarcasm/process.o
libreadline_objs = lib/sarcasm/readline.o
libforeign_objs = lib/sarcasm/foreign.o

################################################################################
# Output objects
sarcasm = sarcasm
ifeq ($(PLATFORM),linux)
libsarcasm = libsarcasm.so
libprocess = libsarcasm_process.so
libreadline = libsarcasm_readline.so
libforeign = libsarcasm_foreign.so
libraries = $(libprocess) $(libreadline) $(libforeign)
else
libsarcasm = libsarcasm.dylib
libprocess = libsarcasm_process.dylib
libreadline = libsarcasm_readline.dylib
libforeign = libsarcasm_foreign.dylib
libraries = $(libprocess) $(libreadline) $(libforeign)
endif

################################################################################
# Targets
all:	$(libsarcasm) $(sarcasm) $(libraries)

%.o:	%.c $(includes)
	$(CC) -c $(GC_CFLAGS) $(GMP_CFLAGS) $(FFI_CFLAGS) $(CONFIG_CFLAGS) $(ALL_CFLAGS) $< -o $@ 

$(sarcasm_objs):	$(sarcasm_objs:.o=.c) $(includes)
	$(CC) -c $(FFI_CFLAGS) $(ALL_CFLAGS) $< -o $@

$(libprocess_objs):	 $(libprocess_objs:.o=.c) $(includes)
	$(CC) -c $(FFI_CFLAGS) $(ALL_CFLAGS) -o $@ $<

$(libreadline_objs):	$(libreadline_objs:.o=.c) $(includes)
	$(CC) -c $(FFI_CFLAGS) $(ALL_CFLAGS) -o $@ $<

$(libforeign_objs):	$(libforeign_objs:.o=.c) $(includes)
	$(CC) -c $(FFI_CFLAGS) $(ALL_CFLAGS) $< -o $@

$(libsarcasm):	$(libsarcasm_objs)
	$(CC) $(ALL_LDFLAGS) $(GC_LDFLAGS) $(GMP_LDFLAGS) $(FFI_LDFLAGS) $(LIB_LDFLAGS) $^ -o $@

$(libprocess):	$(libprocess_objs)
	$(CC) $(ALL_LDFLAGS) $(GC_LDFLAGS) -L. -lsarcasm $(LIB_LDFLAGS) $^ -o $@

$(libreadline):	$(libreadline_objs)
	$(CC) $(ALL_LDFLAGS) $(GC_LDFLAGS) $(READLINE_LDFLAGS) -L. -lsarcasm $(LIB_LDFLAGS) -o $@ $^

$(libforeign):	$(libforeign_objs)
	$(CC) $(ALL_LDFLAGS) $(GC_LDFLAGS) $(FFI_LDFLAGS) -L. -lsarcasm $(LIB_LDFLAGS) $^ -o $@

$(sarcasm):	$(sarcasm_objs) $(libsarcasm)
	$(CC) -L. -lsarcasm $< -o $@ $(LDFLAGS)
ifeq ($(PLATFORM), linux)
	patchelf --set-rpath '$$ORIGIN' $@
endif

check:	$(sarcasm)
	@SCM_LIBRARY_PATH=./lib ./$(sarcasm) -l ./tests/r7rs.scm

install: all
	$(MKDIR) $(bindir)
	$(INSTALL_PROGRAM) $(sarcasm) $(bindir)/
	$(MKDIR) $(libdir)
	$(INSTALL_PROGRAM) $(libsarcasm) $(libdir)/
	$(INSTALL_PROGRAM) $(libraries) $(libdir)/
	$(MKDIR) $(scmlibdir) $(scmlibdir)/sarcasm $(scmlibdir)/scheme
	$(INSTALL_DATA) lib/sarcasm/*.scm $(scmlibdir)/sarcasm/
	$(INSTALL_DATA) lib/scheme/*.scm $(scmlibdir)/scheme/

uninstall:
	-$(RM) $(bindir)/$(sarcasm)
	-$(RM) $(libdir)/$(libsarcasm)
	-$(CD) $(libdir) && $(RM) $(modules)
	-$(RM) $(libdir)/$(libprocess)
	-$(RM) $(libdir)/$(libreadline)
	-$(RM) $(scmlibdir)/sarcasm/*.scm
	-$(RMDIR) $(scmlibdir)/sarcasm
	-$(RM) $(scmlibdir)/scheme/*.scm
	-$(RMDIR) $(scmlibdir)/scheme
	-$(RMDIR) $(scmlibdir)
	-$(RMDIR) $(datadir)/$(name)

.PHONY:	clean 
clean:
	-$(RM) $(sarcasm) $(sarcasm_objs) $(libsarcasm) $(libsarcasm_objs) $(libprocess_objs) $(libprocess) $(libreadline_objs) $(libreadline) $(libforeign)

