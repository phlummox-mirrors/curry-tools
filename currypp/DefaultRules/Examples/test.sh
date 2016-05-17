#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

ALLTESTS="BreakWhere BubbleSort ColorMap DutchFlag FixInt FloatString Guards ListFuns Lookup Nim ParOr Queens Rev2 WorldCup ParOrDet BubbleSortDet DutchFlagDet"

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
# The option -m40 is necessary for testing the determinism properties,
# otherwise the search space is too big for some tests.
CCOPTS=-m40

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
