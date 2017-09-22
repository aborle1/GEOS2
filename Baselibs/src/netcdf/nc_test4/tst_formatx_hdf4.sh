#!/bin/sh
# This shell script tests the output several previous tests.
# $Id$

FILE=tst_interops2.h4

ECODE=0

echo ""
echo "*** Testing extended file format output."
set -e

echo "Test extended format output for a HDF4 file"
rm -f tmp
../ncdump/ncdump -K $FILE >tmp
if ! fgrep 'HDF4 mode=00001000' <tmp ; then
TMP=`cat tmp`
echo "*** Fail: extended format for an HDF4 file: result=" $TMP
ECODE=1
fi

rm -f tmp

exit $ECODE


