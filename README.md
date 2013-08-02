Evolutionary Program Optimization
=================================

During compilation and linking, non-functional properties of software
such as running time and executable size may be optimized.  Current
techniques rely on operations which may be formally proven to preserve
program semantics.

Using a test-suite based definition of program behavior we are able to
apply non-semantic preserving mutations to software resulting in
program variants not reachable through semantic preserving operation.
Due to the inherent mutational robustness of software [1], many of
these mutations will change the runtime behavior of software without
changing the specification to which the software conforms.

Some program variants will have desirable non-functional properties
such as faster running times, reduced energy consumption or a smaller
executable size.  By assigning fitness to variants based on these
characteristics it is possible to optimize software.

Modern system emulators and profilers allow fine-grained monitoring of
aspects of program execution, such as energy consumption and
communication overhead, which may be difficult to predict a-priori.
This repository uses Graphite [2] and Linux perf [3] to measure
non-functional properties of program variants in an EC system for
software optimization.

This repository supports a couple of benchmark suites.  The PARSEC
benchmark suite [4] focuses on emerging workloads.  The "Computer
Language Benchmarks Game" [5] holds a number of simpler more
traditional benchmark programs implemented in multiple languages.
Partial support is provided for working with the SPEC benchmark suite
[6] which stresses a systems "processor, memory subsystem and
compiler".  Currently PARSEC has the most complete support.

Repository Layout
=================

        README | this file
         NOTES | working notes and reproduction instructions
       COPYING | standard GPLV3 License
    benchmarks | holds benchmark programs, input and output
           bin | shell scripts to run experiments and collect results
           etc | miscellaneous support files
       results | experimental results
           src | lisp source for main optimization programs

Installation and Usage
======================

Clone this repository.  To avoid a downloading a large amount of
historical data, use the `--single-branch` option to `git clone` as
follows.

    git clone --single-branch git://github.com/eschulte/optimization.git

The evolution toolkit which we'll use to evolve programs is written in
Common Lisp.  Each optimized program also requires a shell script test
driver, and a test harness (used to limit resources consumed by
evolved variants) is written in C.  Assuming you already have both
bash and a C compiler on your system, the following additional tools
will need to be installed.

1. Steel Bank Common Lisp (SBCL) [7] or Clozure Common Lisp (CCL) [8].

2. The Quicklisp [9] Common Lisp package manager which will be used to
   install all of the required lisp packages.  Follow the instructions
   on the Quicklisp site to install it.

3. Under the directory to which quicklisp has been installed (by
   default `~/quicklisp`), there will be a `local-projects` directory.
   Clone the following git repositories into this directory.

        git clone git://github.com/eschulte/curry-compose-reader-macros.git
        git clone git://github.com/eschulte/software-evolution.git
        git clone git://github.com/eschulte/delta-debug.git
        git clone git://github.com/eschulte/diff.git

   You will also need to symlink this repository into your
   `local-projects` directory.

        ln -s $(pwd) ~/quicklisp/local-projects/

   Finally, ensure Quicklisp has been added to your init file, and
   then use Quicklisp to register these newly cloned local projects.

        (ql:add-to-init-file)
        (ql:register-local-projects)

4. Once Quicklisp and these dependencies have all been installed, run
   the following to install the OPTIMIZE package and all of its
   dependencies.

        (ql:quickload :optimize)

   It may also be necessary to explicitly load some additional
   dependencies with the following.

        (ql:quickload :trivial-gray-streams)
        (ql:quickload :lhstats)

5. Checkout the following tool for the protected execution of shell
   commands through the file system.  This serves to isolate the
   evolutionary process from the many errors thrown during extremely
   long-running optimization runs, the accumulation of which can
   occasionally stall the lisp process.  From the base of this
   directory run the following to clone sh-runner.

        git clone git://github.com/eschulte/sh-runner.git

6. At this point it is possible to run program optimization from the
   lisp REPL as described below.  To build a command line program
   optimization executable, install cl-launch [10] and then run make.

### Make Variables

The following variables may be used to control the behavior of make.

- The `LISP` variable may be set to `sbcl` or `ccl` to compile
  executables with Steel Bank Common Lisp or Clozure Common Lisp
  respectively.

- The `QUICK_LISP` variable may be set to point to a custom quicklisp
  installation directory.  The default value is `$HOME/quicklisp/`.

- The `LISP_STACK` variable may be used to set the maximum amount of
  memory available to the `optimize` executable when compiled with
  SBCL.  Large programs, especially when annotated (e.g., with
  `src/configs/use-annotation.lisp`) may require large amounts of
  memory.  For example run the following to build the `optimize`
  executable with 30G of memory.

         make bin/optimize LISP_STACK=$((30 * 1024))

- The `LISP_PKGS` variable may be used to include additional packages
  into compiled executables.  For example to compile the `iolib`
  package into the `optimize` executable for socket communication
  (e.g., with `src/configs/by-flag.lisp`), run the following.

         make bin/optimize LISP_PKGS=iolib

Optimization at the Command Line
--------------------------------

At this point everything needed has been installed.  The following
steps walk through optimizing `nbody` from the command line to reduce
energy consumption.

1. Run the `optimize` executable once to view all of the optional
   arguments.  All scripts and executables in the `./bin/` directory
   print help information in response to the `-h` flag.

        ./bin/optimize -h

2. Compile `nbody` to assembly and generate the test input and oracle
   output files.

        ./bin/mgmt output nbody

3. Run a test of the `nbody` executable to ensure everything is
   working and to see the output available to our fitness function.

        ./bin/run nbody ./benchmarks/nbody/nbody -t

4. Optimize `nbody` to reduce runtime.

        ./bin/optimize "./bin/run nbody ~a -t" benchmarks/nbody/nbody.s \
          -l gcc -L -lm -F real -f 256 -p 128 -P 64 -t 2

   The options specify that `gcc` should be used as the linker (this
   option could be omitted as `gcc` is the default linker), that the
   `-lm` flag should be passed to `gcc` during linking.  By passing
   `real` to `-F` we specify that we want to our fitness function to
   minimize the real time taken to run this program.  The remaining
   flags specify 256 total fitness evaluations should be run, a
   population of size 128 should be used, periodic checkpoints should
   be written every 64 fitness evaluations, and optimization should be
   distributed across 2 threads.

Interactive Optimization at the REPL
------------------------------------

See `src/repl/example.lisp`, which demonstrates how these tools may be
run interactively from the common lisp REPL.  The evolving population,
and many important evolutionary parameters are exposed as global
variables for live analysis and modification during interactive runs.

Footnotes
=========

[1]  http://arxiv.org/abs/1204.4224

[2]  http://groups.csail.mit.edu/carbon/?page_id=111

[3]  https://perf.wiki.kernel.org/index.php/Main_Page

[4]  http://parsec.cs.princeton.edu/

[5]  http://benchmarksgame.alioth.debian.org/

[6]  http://www.spec.org/cpu2006/

[7]  http://www.sbcl.org/

[8]  http://ccl.clozure.com/

[9]  http://www.quicklisp.org/beta/

[10] http://www.cliki.net/cl-launch
