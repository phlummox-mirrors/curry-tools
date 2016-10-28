#!/bin/sh
# Shell script to test the current set of examples

CURRYBIN="../../../bin"

if [ -x "$CURRYBIN/pakcs" ] ; then
    BACKEND=`$CURRYBIN/curry :set v0 :set -time :load Distribution :eval "putStrLn (curryRuntime ++ show curryRuntimeMajorVersion)" :quit 2> /dev/null`
    # ignore this test for PAKCS/sicstus3 due to segmentation violations:
    if [ "$BACKEND" = sicstus3 ] ; then exit ; fi
fi

ALLTESTS="testData2Xml"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

# execute all tests:
LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYBIN/curry check $CCOPTS $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/curry check $CCOPTS $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in curry check:"
    cat $LOGFILE
    rm $LOGFILE
    exit 1
  else
    rm $LOGFILE
  fi
fi
