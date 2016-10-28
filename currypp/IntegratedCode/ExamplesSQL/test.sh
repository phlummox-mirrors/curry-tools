#!/bin/sh
# script to test the examples for the SQL preprocessor:

CURRYBIN="../../../../bin"

ALLTESTS="test*.curry"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

cleandir () {
  $CURRYBIN/cleancurry
  /bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
  /bin/rm -f Uni_ERDT.term Uni_SQLCode.info Uni_CDBI.curry Uni.db
  $CURRYBIN/cleancurry
}

# compile and execute all tests:
exectests() {
  cleandir
  # compile model:
  $CURRYBIN/erd2cdbi Uni_ERD.term `pwd`/Uni.db  
  # fill database:
  $CURRYBIN/curry :l CreateData :eval createTestData :q
  # run query tests:
  $CURRYBIN/curry check SelectExamples
}

LOGFILE=xxx$$
if [ $VERBOSE = yes ] ; then
  exectests
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  exectests > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR during testing occurred:"
    cat $LOGFILE
    exit 1
  fi
fi
cleandir
