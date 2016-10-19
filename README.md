Curry Tools
===========

This directory contains various tools for Curry
that are used by different Curry systems, like PAKCS or KiCS2.

Currently, it contains the following tools:

`addtypes`:
A tool that adds type signatures to a given Curry program.

`analysis`:
A directory containing various analyses for Curry programs.
These are used in the `CASS` and `currydoc` tools.

`browser`:
A tool to browse through the modules and functions of a Curry program,
show them in various formats, and analyze their properties.

`casc`:
A style checker for Curry programs.

`CASS`:
This directory contains the implementation of the
Curry Analysis Server System, a generic and distributed analysis system
for Curry programs.

`createmakefile`:
A tool to create a simple makefile for a Curry application.

`curry2js`:
A compiler for Curry into JavaScript programs used in the
generation of web user interfaces (WUIs).

`currycheck`:
A property test tool for Curry programs.

`currydoc`:
A documentation generator for Curry programs.

`currypp`:
A preprocessor for Curry programs implementing integrated code,
default rules, deterministic functions, and dynamic contract checking.

`currytest`:
A test tool for Curry programs.

`ertools`:
Compilers to translate database (ER) models
into Curry programs providing high-level access to relational databases.

`importcalls`:
A tool to show all calls to imported functions in a module.

`optimize`:
The implementation of optimization tools cor Curry,
in particular, a transformation tool to replace Boolean equalities
by unification constraints.

`runcurry`:
The implementation of the command `runcurry`.

`spicey`:
The implementation of Curry's web framework Spicey.

`verification`:
This directory contains tools to support the verification of
Curry programs, e.g., a translator of Curry programs into Agda.

`xmldata`:
This directory contains a tool to generate conversion functions
between data types and XML representations. The corresponding
tool is called by the command `data2xml` in the binary directory
of the Curry system.
