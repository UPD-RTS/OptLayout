# OptLayout
Incremental memory layout optimizer (for instruction caches)

Produces a custom linker script that optimizes the memory layout with respect to the I-cache behaviour.

The project is defined in a gpr project optlayout.gpr and a hierarchy of folders:
 - src: the main directory with all the source files
 - obj: the build directory
 - dist: the target directory with some examples

The only mandatory input is the XML representation of the program under analysis.
Currently the tool works on the XML representation produced by the BoundT timing analysis tool,
as it was modified for the PEAL project (back in 2008).

The simplest invocation of the program would be something like:  $ ./optLayout main.xml

Other optional inputs include:
 - The actual Executable
 - XML with all loop bounds (see examples in dist) - note that they are not automatically retrieved (sometimes I derived them form BoundT)
 - XML describing the cache structure ( a default 4-ways set-associative otherwise)
 - XML providing a set of constraints (typically form previous run of the tool)

Defining no bounds implies that a default maximum number of iterations is assumed.

The main operation mode consists in:
 - Rebuilding the program control flow graph (CFG)
 - Build teh Loop Control Tree (LCT)  - eventually transform it to a Weighted Call Graph (WCG) if required
 - Compute an optimized layout
 - Output the custom linker script

Various debug, output and configuration options are available: all flags are explained in "global.ads"
Invoking ./optLayout -h gives the following output.

USAGE:
optLayout [options] file

INPUTS:
  file                                 XML file describing the program structure

OPTIONS:
  -c <config>                          XML file describing the instruction cache
                                       features (size, line size, associativity)
  -s <constraints>                     XML file describing a set of constraints
  -b <bounds>                          XML file providing loop bounds for the
                                       analysed module
  -p <pathToExec>                      Executable name
  -wcg                                 Performs a WCG-based optimization
  -noLocOpt                            LCT-based optimization with no local greedy placement
  -v -verbose                          Verbose mode
  -d                                   Debug mode
  -g                                   Export debug information in Graphs
  -m                                   Minimal verbosity mode
  -noDraw                              Does not output graph drawings
  -t[cfg|dom|lct|fin|lay|lop|wcg]      Low-level tree debug
  -h -help                             Displays this message


The codebase relies on an old version of XmlAda (v. 3.2.1) .
It uses the "dot" program (graphviz package- www.graphviz.org)
and "ps2pdf" to produce nice graphical outputs.

