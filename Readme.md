# Memtrace viewer

OCaml now has runtime support for statistical memory profiling. Given
a set of callbacks and a sampling rate it will run these callbacks on
a sample of all allocations done by an OCaml program.

Unlike the spacetime memory profiler, this support aims to have low
enough overhead to use in production and does not require any special
compilation options.

*Memtrace* is a library built on top of this support, which generates
compact traces of a program's memory use.

The *Memtrace Viewer* provides a web UI for exploring a trace produced
with the Memtrace library.

## Tutorial

See [this blog
post](https://blog.janestreet.com/finding-memory-leaks-with-memtrace/)
for instructions on how to use memtrace viewer.
