#!/bin/sh

# 1. configure the following two variables:
# target directory where the compiled cgi program should be stored:
WEBSERVERDIR=$HOME/public_html
# makecurrycgi script to deploy the system (included in the PAKCS distribution):
MAKECURRYPATH=XXXCURRYBINXXX/makecurrycgi

# 2. delete the following line
echo "Please configure the deploy.sh script first!"; exit 2

##########################################################################
# here starts the standard script:

# create web directory if necessary:
if [ ! -d $WEBSERVERDIR ] ; then
  echo "Creating web directory '$WEBSERVERDIR'..."
  mkdir $WEBSERVERDIR
  chmod 755 $WEBSERVERDIR
fi

ORIGDIR=`pwd`                          # original directory (builtin)
CODEPATH=$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config

CURRYPATH=$CODEPATH
export CURRYPATH

$MAKECURRYPATH -m main -o $WEBSERVERDIR/spicey.cgi Main.curry

# copy other files (Stylesheets, Images...)
cp -r $ORIGDIR/public/* $WEBSERVERDIR
chmod -R go+rX $WEBSERVERDIR
