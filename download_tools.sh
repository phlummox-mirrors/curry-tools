#!/bin/sh
# This shell script updates some tools by downloading the current version
# from the Curry package repository.
#
# Note that the execution of this script requires an already installed 'cpm'!

##############################################################################
echo "Updating 'currypp'..."
mv currypp/Makefile Makefile.currypp  # keep old Makefile
rm -rf currypp
cpm checkout currypp
cd currypp
cpm install --noexec
rm -rf .git*
rm -rf .cpm/*_cache
rm -rf .cpm/packages/*/.git*
cd .cpm/packages
 CANAV=`ls -d cass-analysis-*`
 mv $CANAV cass-analysis
 CASSV=`ls -d cass-*\.*\.*`
 mv $CASSV cass
 ln -s cass-analysis $CANAV
 ln -s cass $CASSV
 PKGV=`ls -d currycheck-*`
 mv $PKGV currycheck
 ln -s currycheck $PKGV
 PKGV=`ls -d rewriting-*`
 mv $PKGV rewriting
 ln -s rewriting $PKGV
 PKGV=`ls -d verify-*`
 mv $PKGV verify
 ln -s verify $PKGV
cd ../..
cd ..
mv Makefile.currypp currypp/Makefile
echo "'currypp' updated from package repository."

##############################################################################
echo "Updating 'currycheck'..."
mv currycheck/Makefile Makefile.currycheck  # keep old Makefile
rm -rf currycheck
cpm checkout currycheck
cd currycheck
cpm install --noexec
rm -rf .git*
rm -rf .cpm/*_cache
rm -rf .cpm/packages/*/.git*
cd .cpm/packages
 PKGV=`ls -d rewriting-*`
 mv $PKGV rewriting
 ln -s rewriting $PKGV
cd ../..
# Generate package configuration file:
CCCONFIG=src/CurryCheckConfig.curry
echo "module CurryCheckConfig where"     > $CCCONFIG
echo "import Distribution(installDir)"  >> $CCCONFIG
echo "import FilePath(combine)"         >> $CCCONFIG
echo "packageVersion :: String"         >> $CCCONFIG
echo "packageVersion = \"1.0.1\""       >> $CCCONFIG
echo "packagePath :: String"            >> $CCCONFIG
echo "packagePath = combine installDir (combine \"currytools\" \"currycheck\")" >> $CCCONFIG
cd ..
mv Makefile.currycheck currycheck/Makefile
echo "'currycheck' updated from package repository."

##############################################################################
echo "Updating 'browser'..."
mv browser/Makefile Makefile.browser  # keep old Makefile
rm -rf browser
cpm checkout currybrowse
mv currybrowse browser
cd browser
cpm install --noexec
rm -rf .git* package.json
rm -rf .cpm/*_cache
rm -rf .cpm/packages/*/.git*
cd .cpm/packages
 CANAV=`ls -d cass-analysis-*`
 mv $CANAV cass-analysis
 CASSV=`ls -d cass-*\.*\.*`
 mv $CASSV cass
 ln -s cass-analysis $CANAV
 ln -s cass $CASSV
 PKGV=`ls -d addtypes-*`
 mv $PKGV addtypes
 ln -s addtypes $PKGV
 PKGV=`ls -d importusage-*`
 mv $PKGV importusage
 ln -s importusage $PKGV
 PKGV=`ls -d showflatcurry-*`
 mv $PKGV showflatcurry
 ln -s showflatcurry $PKGV
cd ../..
# Generate package configuration file:
CBCONFIG=src/BrowsePackageConfig.curry
echo "module BrowsePackageConfig where"  > $CBCONFIG
echo "import Distribution(installDir)"  >> $CBCONFIG
echo "import FilePath(combine)"         >> $CBCONFIG
echo "packageVersion :: String"         >> $CBCONFIG
echo "packageVersion = \"0.5.0\""       >> $CBCONFIG
echo "packagePath :: String"            >> $CBCONFIG
echo "packagePath = combine installDir (combine \"currytools\" \"browser\")" >> $CBCONFIG
cd ..
mv Makefile.browser browser/Makefile
echo "'browser' updated from package repository."

##############################################################################
echo "Updating 'ertools'..."
mv ertools/Makefile Makefile.ertools  # keep old Makefile
rm -rf ertools
cpm checkout ertools
cd ertools
rm -rf .cpm .git*
# Generate package configuration file:
ERTCONFIG=src/ERToolsPackageConfig.curry
echo "module ERToolsPackageConfig where" > $ERTCONFIG
echo "import Distribution(installDir)"  >> $ERTCONFIG
echo "import FilePath(combine)"         >> $ERTCONFIG
echo "packageVersion :: String"         >> $ERTCONFIG
echo "packageVersion = \"1.0.0\""       >> $ERTCONFIG
echo "packagePath :: String"            >> $ERTCONFIG
echo "packagePath = combine installDir (combine \"currytools\" \"ertools\")" >> $ERTCONFIG
cd ..
mv Makefile.ertools ertools/Makefile
echo "'ertools' updated from package repository."

##############################################################################
echo "Updating 'optimize'..."
mv optimize/Makefile Makefile.optimize  # keep old Makefile
rm -rf optimize
cpm checkout transbooleq
mv transbooleq optimize
cd optimize
cpm install --noexec
rm -rf .git*
rm -rf .cpm/*_cache
rm -rf .cpm/packages/*/.git*
cd .cpm/packages
 CANAV=`ls -d cass-analysis-*`
 mv $CANAV cass-analysis
 CASSV=`ls -d cass-*\.*\.*`
 mv $CASSV cass
 ln -s cass-analysis $CANAV
 ln -s cass $CASSV
cd ../..
cd ..
mv Makefile.optimize optimize/Makefile
echo "'optimize' updated from package repository."

##############################################################################
echo "Updating 'cpm'..."
mv cpm/Makefile Makefile.cpm  # keep old Makefile
rm -rf cpm
cpm checkout cpm
cd cpm
rm -rf .git* bin package.json
make fetchdeps
rm -rf vendor/*/.git*
rm -rf dependencies.txt fetch-dependencies.sh Makefile
cd ..
mv Makefile.cpm cpm/Makefile
echo "'cpm' updated from package repository."
