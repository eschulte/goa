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
LISP_EXES=optimize objread calc-energy annotate
LISP_BINS=$(addprefix bin/, $(LISP_EXES))
LISP_DEPS=src/package.lisp src/optimize.lisp src/annotate.lisp etc/cl-launch.lisp

# Compiled C executables
C_EXES=limit no-limit
C_BINS=$(addprefix bin/, $(C_EXES))

# Flags to build standalone executables
CLFLAGS=--no-include --system optimize --lisp $(LISP) --dump '!' -f etc/cl-launch.lisp

ifneq ($(LISP_STACK),)
	LISP=sbcl
	CLFLAGS+= --wrap 'SBCL_OPTIONS="--dynamic-space-size $(LISP_STACK)"'
endif

all: $(C_BINS) $(LISP_BINS)

etc/cl-launch.lisp:
	echo "(load \"$(QUICK_LISP)/setup.lisp\")" >$@

bin/%: src/%.lisp $(LISP_DEPS)
	$(CLC) $(CLFLAGS) --output $@ -r optimize:$*

bin/limit: bin/limit.c
	$(CC) $< -o $@

bin/no-limit: bin/no-limit.c
	$(CC) $< -o $@

clean:
	rm -f etc/cl-launch.lisp $(C_BINS) $(LISP_BINS)

real-clean: clean
	rm -f **/*.fasl **/*.lx32fsl
