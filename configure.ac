#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([waste], [0.1.1])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([lib/cpp/src/RabbitMqAmqpImpl.h])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_AWK
AC_PROG_MKDIR_P
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_LANG([C++])
AX_CXX_COMPILE_STDCXX_11([noext], [mandatory])

AX_BOOST_BASE([1.40.0])
AX_THRIFT([])
AX_RABBITMQ_C([])

# Check for erlang
AC_MSG_CHECKING(for erlang)
found_erlang=no
check_erlang() {
    if test "$found_erlang" = "yes"
    then
      return
    fi
    AC_ERLANG_PATH_ERL
    AC_ERLANG_PATH_ERLC
    if test -n "$ERLC" ; then
       AC_ERLANG_SUBST_LIB_DIR
       # Install into the detected Erlang directory instead of $libdir/erlang/lib
       ERLANG_INSTALL_LIB_DIR="$ERLANG_LIB_DIR"
       AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)
    fi
    if test -n "$ERL" -a -n "$ERLC" ; then
       found_erlang="yes"
       AC_MSG_RESULT($ERL)
    fi
}

check_erlang

if test "$found_erlang" = "no"
then
  AC_MSG_ERROR(could not find erlang)
fi

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h syslog.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_CHECK_FUNCS([strerror])

AC_CONFIG_FILES([Makefile
                 deps/Makefile
                 lib/Makefile
                 lib/cpp/Makefile
                 lib/erl/Makefile                 
                 test/Makefile
                 test/cpp/Makefile
                 test/erl/Makefile])
AC_OUTPUT
