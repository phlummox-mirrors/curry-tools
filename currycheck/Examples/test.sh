#!/bin/sh
# Shell script to test the current set of examples

CURRYBIN="../../../bin"

ALLTESTS="DefaultRulesTest DetOperations ExampleTests ExamplesFromManual ListSpecifications Nats SEBF Sum SortSpec Tree"

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
# The option -m70 is necessary for testing the determinism properties,
# otherwise the search space is too big for some tests.
CCOPTS=-m70

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

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
