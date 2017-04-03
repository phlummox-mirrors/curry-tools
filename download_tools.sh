#!/bin/sh
# This shell script updates some tools by downloading the current version
# from the Curry package repository.
#
# Note that the execution of this script requires an already installed 'cpm'!

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
echo "packagePath = installDir ++ combine \"currytools\" \"ertools\"" >> $ERTCONFIG
cd ..
mv Makefile.ertools ertools/Makefile
echo "'ertools' updated from package repository."

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
 mv cass-analysis-* cass-analysis
 mv cass-*\.*\.* cass
cd ../..
cd ..
mv Makefile.optimize optimize/Makefile
echo "'optimize' updated from package repository."

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
