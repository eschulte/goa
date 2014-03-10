Genetic Optimization Algorithm (GOA)
====================================

A post-compilation optimization tool capable of optimizing myriad
aspects of program runtime behavior.  The three required inputs are
(1) compiled program assembly code, (2) a test workload used to
exercise candidate optimizations, and (3) a fitness function used to
score runtime behavior.  As opposed to compiler optimizations which
maintain program semantics and typically target only executable speed
and size, this technique is capable of addressing any measurable
aspect of runtime behavior and may change program semantics.

Assembler code is modified using generic program transformations,
taken from Genetic Programming, yielding candidate optimizations.  The
fitness of candidates are determined using by running on the workload
and combining performance metrics with the fitness function.  Due to
the inherent mutational robustness of software [1], many of these
mutations will change the runtime behavior of software without
changing the specification to which the software conforms.  Some
candidates will have desirable non-functional properties such as
faster running times, reduced energy consumption or a smaller
executable size.

Modern system emulators and profilers (e.g., Linux perf [2]) allow
fine-grained monitoring of aspects of program execution.  Fitness
functions combine these measurements to model aspects of program
execution such as energy consumption and communication overhead, which
may be difficult to predict a-priori.

This repository supports multiple benchmark suites.  The PARSEC
benchmark suite [3] focuses on emerging workloads.  The "Computer
Language Benchmarks Game" [4] holds a number of simpler more
traditional benchmark programs implemented in multiple languages.
Partial support is provided for working with the SPEC benchmark suite
[5] which stresses a system's "processor, memory subsystem and
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

    git clone --single-branch git://github.com/eschulte/goa.git

The evolution toolkit which we'll use to evolve programs is written in
Common Lisp.  Each optimized program also requires a shell script test
driver, and a test harness (used to limit resources consumed by
evolved variants) is written in C.  Assuming you already have both
bash and a C compiler on your system, the following additional tools
will need to be installed.

1. Steel Bank Common Lisp (SBCL) [6] or Clozure Common Lisp (CCL) [7].

2. The Quicklisp [8] Common Lisp package manager which will be used to
   install all of the required lisp packages.  Follow the instructions
   on the Quicklisp site to install it.

3. Under the directory to which quicklisp has been installed (by
   default `~/quicklisp`), there will be a `local-projects` directory.
   Clone the following git repositories into this directory.

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
   optimization executable, install buildapp [9] and then run make.

### Make Variables

The following variables may be used to control the behavior of make.

- The `QUICK_LISP` variable may be set to point to a custom quicklisp
  installation directory.  The default value is `$HOME/quicklisp/`.

- The `LISP_STACK` variable may be used to set the maximum amount of
  memory available to the `goa` executable when compiled with SBCL.
  Large programs, especially when annotated (e.g., with
  `src/configs/use-annotation.lisp`) may require large amounts of
  memory.  For example run the following to build the `goa` executable
  with 30G of memory.

         make bin/goa LISP_STACK=$((30 * 1024))

- The `LISP_LIBS` variable may be used to include additional packages
  into compiled executables.  For example to compile the `iolib`
  package into the `goa` executable for socket communication (e.g.,
  with `src/configs/by-flag.lisp`), run the following.

         make bin/goa LISP_LIBS=iolib

Optimization at the Command Line
--------------------------------

At this point everything needed has been installed.  The following
steps walk through optimizing `swaptions` from the command line to
reduce runtime.  To run this example either a `time` executable which
supports the `-p` and `-o` options (*not* the shell built in), or
`perf` is required.

1. Run the `goa` executable once to view all of the optional
   arguments.  All scripts and executables in the `./bin/` directory
   print help information in response to the `-h` flag.

        ./bin/goa -h

2. Compile `swaptions` to assembly and generate the test input and
   oracle output files.  Note, the first time this is run it will
   download and unpack the PARSEC benchmarks which may take some time.

        ./bin/mgmt output swaptions

3. Run a test of the `swaptions` executable to ensure everything is
   working and to see the output available to our fitness function.
   If using `time` run the following,

        ./bin/run swaptions ./benchmarks/swaptions/swaptions -t

   If using `perf` run the following.

        ./bin/run swaptions ./benchmarks/swaptions/swaptions -p

4. Optimize `swaptions` to reduce runtime.  If using `time` run the
   following.

        ./bin/goa "./bin/run swaptions ~a -t" \
          benchmarks/swaptions/swaptions.s \
          -l g++ -L "-lm -pthread -DENABLE_THREADS" \
          -F real -f 256 -p 128 -P 64 -t 2 -r swap

    If using `perf` run the following.

        ./bin/goa "./bin/run swaptions ~a -p" \
          benchmarks/swaptions/swaptions.s \
          -l g++ -L "-lm -pthread -DENABLE_THREADS" \
          -F seconds -f 256 -p 128 -P 64 -t 2 -r swap

   The `-l` option specifies that `g++` should be used as the linker
   (`gcc` is the default linker), and that the flags `"-lm -pthread
   -DENABLE_THREADS"` should be passed to `g++` during linking.  By
   passing `real` (or `seconds`) to `-F` we specify that we want our
   fitness function to minimize the time taken to run this program.
   The remaining flags specify 256 total fitness evaluations should be
   run (`-f`), a population of size 128 should be used (`-p`),
   periodic checkpoints should be written every 64 fitness evaluations
   (`-P`), optimization should be distributed across 2 threads (`-t`),
   and results should be saved in a directory named `swap` (`-r`).

5. When repair complete the name of the results directory will be
   printed.  In this directory the `final-best.store` file holds the
   optimized program.  This may be compiled to an optimized executable
   with the following (see the `-h` output of `objread` for more ways
   to use `.store` files).

        ./bin/objread swap/final-best.store

Interactive Optimization at the REPL
------------------------------------

See `src/repl/example.lisp`, which demonstrates how these tools may be
run interactively from the common lisp REPL.  The evolving population,
and many important evolutionary parameters are exposed as global
variables for live analysis and modification during interactive runs.

Footnotes
=========

[1] http://arxiv.org/abs/1204.4224

[2] https://perf.wiki.kernel.org/index.php/Main_Page

[3] http://parsec.cs.princeton.edu/

[4] http://benchmarksgame.alioth.debian.org/

[5] http://www.spec.org/cpu2006/

[6] http://www.sbcl.org/

[7] http://ccl.clozure.com/

[8] http://www.quicklisp.org/beta/

[9] http://www.xach.com/lisp/buildapp/
