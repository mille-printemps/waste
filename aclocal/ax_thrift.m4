AC_DEFUN([AX_THRIFT],
[
AC_MSG_CHECKING(for thrift)
succeeded=no

for ac_thrift_path_tmp in /usr /usr/local /opt /opt/local ; do
    if test -d "$ac_thrift_path_tmp/include/thrift" && test -r "$ac_thrift_path_tmp/include/thrift"; then
        THRIFT_LDFLAGS="-L$ac_thrift_path_tmp/lib"
		THRIFT_CPPFLAGS="-I$ac_thrift_path_tmp/include/thrift"
		break;
	fi
done

CPPFLAGS_SAVED="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $THRIFT_CPPFLAGS"
export CPPFLAGS

LDFLAGS_SAVED="$LDFLAGS"
LDFLAGS="$LDFLAGS $THRIFT_LDFLAGS"
export LDFLAGS

THRIFT_LIBS="-lthrift"

AC_LANG_PUSH([C++])
   	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
@%:@include <thrift/Thrift.h>
]], [[
]])],[
    AC_MSG_RESULT(yes)
	succeeded=yes
    ],[
    ])
AC_LANG_POP([C++])

if test "$succeeded" != "yes" ; then
   	AC_MSG_ERROR([[We could not detect the thrift libraries.]])
else
	AC_SUBST(THRIFT_CPPFLAGS)
	AC_SUBST(THRIFT_LDFLAGS)
    AC_SUBST(THRIFT_LIBS)
	AC_DEFINE(HAVE_THRIFT,,[define if the Thrift library is available])
fi

CPPFLAGS="$CPPFLAGS_SAVED"
LDFLAGS="$LDFLAGS_SAVED"
])
