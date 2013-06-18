# Build uses cl-launch
CLC:=cl-launch

ifneq ($(LISP_STACK),)
ifneq ($(LISP),)
$(error LISP_STACK overrides LISP forcing use of sbcl.  Unset LISP_STACK or LISP)
endif
endif
LISP:="sbcl ccl"

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/

# Compiled lisp executables
LISP_EXES=optimize objread calc-energy variance-by-size
LISP_BINS=$(addprefix bin/, $(LISP_EXES))

# Flags to build standalone executables
CLFLAGS=--no-include --system optimize --lisp $(LISP) --dump '!' -f etc/cl-launch.lisp

ifneq ($(LISP_STACK),)
	LISP=sbcl
	CLFLAGS+= --wrap 'SBCL_OPTIONS="--dynamic-space-size $(LISP_STACK)"'
endif

all: bin/no-limit bin/no-stack-limit bin/limit $(LISP_BINS)

etc/cl-launch.lisp:
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

bin/optimize: src/run-optimize.lisp etc/cl-launch.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:main

bin/objread: src/objread.lisp etc/cl-launch.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:objread

bin/calc-energy: src/calc-energy.lisp etc/cl-launch.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:calc-energy

bin/variance-by-size: src/variance-by-size.lisp etc/cl-launch.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:variance-by-size

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit etc/cl-launch.lisp $(LISP_BINS)

real-clean: clean
	rm -f **/*.fasl **/*.lx32fsl
