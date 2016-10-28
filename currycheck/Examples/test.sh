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
# this option must be decreased for PAKCS/SWI due to memory overflows:
if [ -x "$CURRYBIN/pakcs" ] ; then
  BACKEND=`$CURRYBIN/curry :set v0 :set -time :load Distribution :eval "putStrLn curryRuntime" :quit 2> /dev/null`
  if [ "$BACKEND" = swi ] ; then
    CCOPTS=-m50
  fi
fi

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
    exit 1
  fi
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
