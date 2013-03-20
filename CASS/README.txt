CASS: The Curry Analysis Server System
======================================

This directory contains the implementation of CASS,
a generic and distributed analysis system for Curry programs.

The analysis system is structured as a client-server application
where the worker clients analyse individual modules.

The analysis system can also be used as a client from other
application programs by a socket interface.
The protocol of this interface is described in `Protocol.txt`.
The server is explicitly started by the program `cass`
(generated via `make`) or implicitly by application programs
that use of the operation `Configuration.getServerPortNumber`
to find out the port number to connect to the analysis server
(the port and process number of a running analysis server
are temporarily stored in the file `$HOME/.curryanalysis.port`).

The program `cass` can also be started with arguments
(the analysis name and the name of the main module)
in order to analyze a module directly without the use
of the server protocol (run `cass --help` to get a description
of the arguments).

The analysis system can be configured in the file `$HOME/.curryanalysisrc`
which is installed after the first run of the system.
The implementations of the individual analysis are
usually  stored in the directory `analysis`.

Description of some Curry files:

* `AnalysisCollection`: All available analyses must be registered here.
* `Analysis`: Base type to define an analysis.
* `AnalysisServer`: The main module implementing the use of the server.
* `ServerFormats`: Definition and implementation of output formats.
* `WorkerFunctions`: Implementation of the analysis workers
  (in particular, alternative fixpoint iterations to compute
   dependency analyses, see option `fixpoint` in the configuration file,
   must be inserted here).

Contact: Michael Hanus (www.informatik.uni-kiel.de/~mh)
