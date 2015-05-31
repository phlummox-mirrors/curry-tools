#!/bin/sh
# Shell script to test the current set of CHR(Curry) examples
PAKCS=../../../../bin/pakcs
LOGFILE=xxx$$
PATH=`dirname $PAKCS`:$PATH
export PATH
`dirname $PAKCS`/cleancurry
cat << EOM | $PAKCS -q :set -interactive :set v0 :set printdepth 0 :set -free :set +verbose :set -time | tee $LOGFILE
:load BreakWhere
main

:load BubbleSort
main

:l DutchFlag
main
dutch iflag

:l FloatString
main

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
diff TESTRESULT $LOGFILE > $DIFF
if [ "`cat $DIFF`" = "" ] ; then
  echo
  echo "Regression test successfully executed!"
  /bin/rm -f $LOGFILE $DIFF
else
  echo
  echo "Differences in regression test occurred:"
  cat $DIFF
  /bin/rm -f $DIFF
  /bin/mv -f $LOGFILE LOGFILE
  echo "Test output saved in file 'LOGFILE'."
fi
