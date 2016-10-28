#!/bin/sh
# Shell script to test the current set of examples
CURRYBIN=`cd ../../../bin && pwd`

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

/bin/rm -rf spicey_blog
mkdir spicey_blog
cd spicey_blog
$CURRYBIN/curry spiceup ../Blog.erdterm > $LOGFILE 2>&1
echo "Compiling spicey_blog..."
make CURRYOPTIONS="$CURRYOPTIONS" compile >> $LOGFILE 2>&1
if [ $? -gt 0 ] ; then
  echo "ERROR in Spicey generation:"
  cat $LOGFILE
  exit 1
fi
cd ..

#/bin/rm -rf spicey_uni
#mkdir spicey_uni
#cd spicey_uni
#$CURRYBIN/curry spiceup ../Uni.erdterm
#cp ../MyModule.curry models
#make CURRYOPTIONS="$CURRYOPTIONS" compile 2>&1 | tee $LOGFILE
#cd ..

################ end of tests ####################
# Clean:
/bin/rm -rf $LOGFILE spicey_blog spicey_uni
