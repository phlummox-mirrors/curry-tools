#!/bin/sh
# Shell script to test the current set of examples
CURRYBIN="../../../../bin"

if [ -x "$CURRYBIN/pakcs" ] ; then
    CURRYEXEC=pakcs
    CURRYOPTIONS="-q :set v0 :set printdepth 0 :set -free :set +verbose"
elif [ -x "$CURRYBIN/kics2" ] ; then
    CURRYEXEC=kics2
    CURRYOPTIONS=":set v0 :set -ghci"
else
    echo "ERROR: Unknown Curry system!"
    exit 1
fi

LOGFILE=xxx$$
PATH=$CURRYBIN:$PATH
export PATH
$CURRYBIN/cleancurry
cat << EOM | $CURRYBIN/$CURRYEXEC $CURRYOPTIONS :set -interactive :set -time | tee $LOGFILE
:load BreakWhere
main

:load BubbleSort
main

:l DutchFlag
main
dutch iflag

:l FloatString
main

:l Guards
main1
main2

:l Lookup
main1
main2
main3
main4

:l Rev2
main

:l WorldCup
main

EOM
################ end of tests ####################
# CHeck differences:
DIFF=diff$$
diff TESTRESULT.$CURRYEXEC $LOGFILE > $DIFF
if [ "`cat $DIFF`" = "" ] ; then
  echo
  echo "Regression test successfully executed!"
  /bin/rm -f $LOGFILE $DIFF
  $CURRYBIN/cleancurry
else
  echo
  echo "Differences in regression test occurred:"
  cat $DIFF
  /bin/rm -f $DIFF
  /bin/mv -f $LOGFILE LOGFILE
  echo "Test output saved in file 'LOGFILE'."
  $CURRYBIN/cleancurry
  exit 1
fi
