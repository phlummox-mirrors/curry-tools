#!/bin/sh
# Shell script to test the current set of examples
CURRYBIN="../../../bin"

LOGFILE=xxx$$
PATH=$CURRYBIN:$PATH
export PATH
$CURRYBIN/cleancurry
$CURRYBIN/currycheck ExampleTests DefaultRulesTest 2>&1 > $LOGFILE
if [ $? -gt 0 ] ; then
  echo "ERROR in currycheck:"
  cat $LOGFILE
  exit 1
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
