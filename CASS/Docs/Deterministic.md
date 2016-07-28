Analysis of deterministic operations
------------------------------------

This analysis checks whether an operation is deterministically defined.
Intuitively, an operation is deterministic if the evaluation of
this operation applied to ground terms does not cause any non-determinism.
The determinism analysis returns `nondeterministic` for a given operation
if its definition contain overlapping left-hand sides or free variables,
or if it depends on some non-deterministic operation.
