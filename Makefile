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
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

# Compiled lisp executables
LISP_EXES=optimize objread calc-energy model-variance
LISP_BINS=$(addprefix bin/, $(LISP_EXES))
LISP_DEPS=src/package.lisp src/optimize.lisp etc/cl-launch.lisp

# Flags to build standalone executables
CLFLAGS=--no-include --system optimize --lisp $(LISP) --dump '!' -f etc/cl-launch.lisp

ifneq ($(LISP_STACK),)
	LISP=sbcl
	CLFLAGS+= --wrap 'SBCL_OPTIONS="--dynamic-space-size $(LISP_STACK)"'
endif

all: bin/limit $(LISP_BINS)

etc/cl-launch.lisp:
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

bin/optimize: src/run-optimize.lisp $(LISP_DEPS)
	$(CLC) $(CLFLAGS) --output $@ -r optimize:main

bin/objread: src/objread.lisp $(LISP_DEPS)
	$(CLC) $(CLFLAGS) --output $@ -r optimize:objread

bin/calc-energy: src/calc-energy.lisp $(LISP_DEPS)
	$(CLC) $(CLFLAGS) --output $@ -r optimize:calc-energy

bin/model-variance: src/model-variance.lisp $(LISP_DEPS)
	$(CLC) $(CLFLAGS) --output $@ -r optimize:model-variance

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit etc/cl-launch.lisp $(LISP_BINS)

real-clean: clean
	rm -f **/*.fasl **/*.lx32fsl
