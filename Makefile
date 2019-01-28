CC = clang
RM = rm -f

################################################################################
BASE_CFLAGS 		= -g -std=c11 -Wall -Werror -pedantic
BASE_LDFLAGS		=
GC_CFLAGS			= $(shell pkg-config --cflags bdw-gc)
GC_LDFLAGS			= $(shell pkg-config --libs bdw-gc) -lcord
GMP_CFLAGS			= 
GMP_LDFLAGS			= -lgmp
FFI_CFLAGS			= $(shell pkg-config --cflags libffi)
FFI_LDFLAGS			= $(shell pkg-config --libs libffi)
READLINE_CFLAGS 	=
READLINE_LDFLAGS 	= -lreadline

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
				  src/writer.o \

################################################################################
SARCASM		= scm
LIBSARCASM	= libsarcasm.dylib

################################################################################
all:	$(LIBSARCASM) $(SARCASM)

%.o:	%.c $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(GC_CFLAGS) $(GMP_CFLAGS) $(FFI_CFLAGS) $(READLINE_CFLAGS) -o $@ $<

src/main.o:	src/main.c $(INCLUDES)
	$(CC) -c $(BASE_CFLAGS) $(FFI_CFLAGS) -o $@ $<
	
$(LIBSARCASM):	$(LIBSARCASM_OBJS)
	$(CC) $(BASE_LDFLAGS) $(GC_LDFLAGS) $(GMP_LDFLAGS) $(FFI_LDFLAGS) $(READLINE_LDFLAGS) -fPIC -shared -o $@ $^

$(SARCASM):	$(SARCASM_OBJS) $(LIBSARCASM)
	$(CC) $(BASE_LDFLAGS) -L. -lsarcasm -o $@ $<

test:	$(SARCASM)
	@./$(SARCASM) -l ./tests/r7rs.scm

clean:
	-$(RM) $(SARCASM) $(SARCASM_OBJS) $(LIBSARCASM) $(LIBSARCASM_OBJS)

