#!/bin/sh

# Definition of Curry installation to be used:
CURRYEXEC=XXXCURRYEXECXXX

ORIGDIR=`pwd`                          # original directory (builtin)
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
ABSPROGDIR="`cd \"$PROGDIR\" 2>/dev/null && pwd || echo \"$PROGDIR\"`"  # get absolute path

CURRYPATH="$ORIGDIR/views:$ORIGDIR/controllers:$ORIGDIR/models:$ORIGDIR/system:$ORIGDIR/config"
export CURRYPATH

exec $CURRYEXEC :l Main :quit
