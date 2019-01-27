AC_INIT([sarcasm], [0.1])
AM_INIT_AUTOMAKE([foreign])

AC_PROG_CC([clang])
AC_PROG_LIBTOOL
AC_PROG_INSTALL

AC_CHECK_HEADER([stdint.h])
AC_CHECK_HEADER([stdbool.h])
AC_CHECK_HEADER([stdarg.h])
AC_CHECK_HEADER([stdlib.h])
AC_CHECK_HEADER([stdio.h])
AC_CHECK_HEADER([ctype.h])
AC_CHECK_HEADER([string.h])
AC_CHECK_HEADER([math.h])
AC_CHECK_HEADER([setjmp.h])
AC_CHECK_HEADER([unistd.h])
AC_CHECK_HEADER([dlfcn.h])
AC_CHECK_LIB([readline], [readline])
AC_CHECK_LIB([gmp], [__gmpz_init])
PKG_CHECK_MODULES([GC], [bdw-gc])
PKG_CHECK_MODULES([FFI], [libffi])

LT_INIT

scmlibdir="$datarootdir/$PACKAGE_NAME/lib"
AC_SUBST([scmlibdir])

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_FILES([Makefile
                 libsarcasm/Makefile
                 src/Makefile
                 lib/Makefile
                 lib/sarcasm/Makefile
                 lib/scheme/Makefile
                 test/Makefile])
AC_OUTPUT

