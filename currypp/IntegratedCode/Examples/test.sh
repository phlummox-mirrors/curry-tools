#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# test for basic language features
ALLTESTS="testFormat"

# clean up before
$CURRYBIN/cleancurry

# execute all unit tests:
echo "Executing unit tests for Curry code integrator..."
$CURRYBIN/currytest test*.curry
