#!/bin/sh
# Shell script to test the current set of examples
CURRYBIN=`cd ../../../bin && pwd`

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

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

LOGFILE=`pwd`/xxx$$
/bin/rm -f $LOGFILE
PATH=$CURRYBIN:$PATH
export PATH
# $CURRYBIN/cleancurry -r

CURRYOPTIONS=":set -time :set v0 :set parser -Wnone"

compile_spicey()
{
  ERD=$1
  /bin/rm -rf spicey_$ERD
  mkdir spicey_$ERD
  cd spicey_$ERD
  $CURRYBIN/curry spiceup ../"$ERD"ERD.curry
  echo "Compiling spicey_$ERD..."
  make CURRYOPTIONS="$CURRYOPTIONS" compile && cd ..
}

compile_spicey_and_check()
{
  if [ $VERBOSE = yes ] ; then
    compile_spicey $1
    if [ $? -gt 0 ] ; then
      exit 1
    fi
  else
    compile_spicey $1 > $LOGFILE 2>&1
    if [ $? -gt 0 ] ; then
      echo "ERROR in Spicey generation:"
      cat $LOGFILE
      exit 1
    fi
  fi
}

compile_spicey_and_check Blog
# omitted due to bug in controller generation:
#compile_spicey_and_check Uni

################ end of tests ####################
# Clean:
/bin/rm -rf $LOGFILE spicey_Blog spicey_Uni
