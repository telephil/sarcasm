AC_INIT([sarcasm], [0.1])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([foreign])

AC_PROG_CC([clang])
AC_PROG_LIBTOOL

PKG_CHECK_MODULES([GC], [bdw-gc])
PKG_CHECK_MODULES([FFI], [libffi])
AC_CHECK_LIB([readline], [readline])
AC_CHECK_LIB([gmp], [__gmpz_init])

LT_INIT

AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_FILES([Makefile libsarcasm/Makefile src/Makefile])
AC_OUTPUT

