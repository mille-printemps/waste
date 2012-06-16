AC_DEFUN([AX_RABBITMQ_C],
[
AC_MSG_CHECKING(for rabbitmq-c)
succeeded=no

for ac_rabbitmq_c_path_tmp in /usr /usr/local /opt /opt/local ; do
    if test -r "$ac_rabbitmq_c_path_tmp/include/amqp.h" && test -r "$ac_rabbitmq_c_path_tmp/include/amqp_framing.h"; then
       RABBITMQ_C_LDFLAGS="-L$ac_rabbitmq_c_path_tmp/lib"
	   RABBITMQ_C_CPPFLAGS="-I$ac_rabbitmq_c_path_tmp/include"
	   break;
    fi
done

CPPFLAGS_SAVED="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $RABBITMQ_C_CPPFLAGS"
export CPPFLAGS

LDFLAGS_SAVED="$LDFLAGS"
LDFLAGS="$LDFLAGS $RABBITMQ_C_LDFLAGS"
export LDFLAGS

RABBITMQ_C_LIBS="-lrabbitmq"

AC_LANG_PUSH([C++])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
@%:@include <stdint.h>
@%:@include <stdio.h>
@%:@include <amqp.h>
@%:@include <amqp_framing.h>
]], [[
]])],[
    AC_MSG_RESULT(yes)
    succeeded=yes
    ],[
    ])
AC_LANG_POP([C++])

if test "$succeeded" != "yes" ; then
   	AC_MSG_ERROR([[We could not detect the rabbitmq-c libraries.]])
else
	AC_SUBST(RABBITMQ_C_CPPFLAGS)
	AC_SUBST(RABBITMQ_C_LDFLAGS)
    AC_SUBST(RABBITMQ_C_LIBS)
	AC_DEFINE(HAVE_RABBITMQ_C,,[define if the Rabbitmq_c library is available])
fi

CPPFLAGS="$CPPFLAGS_SAVED"
LDFLAGS="$LDFLAGS_SAVED"
])
