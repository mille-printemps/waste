#!/bin/sh

if [ -n "$1" ] && [ "-a"="$1" ]; then
    ./cleanup.sh -a
else
    ./cleanup.sh
fi    

./rebar get-deps

autoscan || exit 1
aclocal -I ./aclocal || exit 1
autoheader || exit 1

if libtoolize --version 1 >/dev/null 2>/dev/null; then
  libtoolize --copy --automake || exit 1
elif glibtoolize --version 1 >/dev/null 2>/dev/null; then
  glibtoolize --copy --automake || exit 1
fi

autoconf
automake -ac --add-missing --foreign || exit 1
