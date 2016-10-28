#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

ALLTESTS="Expressions Grep Half Last Various"

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
  $CURRYBIN/curry check $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/curry check $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in curry check:"
    cat $LOGFILE
    exit 1
  fi
fi

# clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
