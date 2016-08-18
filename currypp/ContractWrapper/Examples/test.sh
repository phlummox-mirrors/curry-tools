#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

ALLTESTS="BubbleSort BubbleSortFormat Coin FibInfinite Quicksort"

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
# No specification testing since the example contains intended errors.
CCOPTS="--nospec --nodet --deftype=Int"
LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYBIN/currycheck $CCOPTS $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/currycheck $CCOPTS $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in currycheck:"
    cat $LOGFILE
    exit 1
  fi
fi

# clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
