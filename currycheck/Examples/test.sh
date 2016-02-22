#!/bin/sh
# Shell script to test the current set of examples

CURRYBIN="../../../bin"

ALLTESTS="ExampleTests ExamplesFromManual DefaultRulesTest SEBF Nats Tree Sum DetOperations SortSpec"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

LOGFILE=xxx$$
if [ $VERBOSE = yes ] ; then
  $CURRYBIN/currycheck $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/currycheck $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in currycheck:"
    cat $LOGFILE
    exit 1
  fi
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
