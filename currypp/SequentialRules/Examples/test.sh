#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

ALLTESTS="BreakWhere BubbleSort DutchFlag FloatString Guards Lookup Rev2 WorldCup"

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

# execute all unit tests:
LOGFILE=xxx$$
$CURRYBIN/currycheck $ALLTESTS 2>&1 > $LOGFILE
if [ $? -gt 0 ] ; then
  echo "ERROR in currycheck:"
  cat $LOGFILE
  exit 1
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
