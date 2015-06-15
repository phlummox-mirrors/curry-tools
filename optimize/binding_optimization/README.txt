This directory contains the implementation of a transformation tool
that replaces Boolean equalities by equational constraints
in FlatCurry programs.

The tool is integrated into the compilation chain of PAKCS/KiCS2.
The ideas of this tool are described in a paper presented at LOPSTR 2015.

Statistics about the number of transformations are shown
with increased verbosity levels. For instance, if one sets the
option "v2" in PAKCS/KiCS2, a summary of the number of transformation
is shown, with option "v3" more details (analysis infos, timings,
and functions where transformations are applied) are shown and
a CSV file with this information is generated.
