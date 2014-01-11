# Use buildapp as the lisp compiler
LC:=buildapp

# You can set this as an environment variable to point to an alternate
# quicklisp install location.  If you do, ensure that it ends in a "/"
# character, and that you use the $HOME variable instead of ~.
QUICK_LISP?=$(HOME)/quicklisp/
ifeq "$(wildcard $(QUICK_LISP)/setup.lisp)" ""
$(warning $(QUICK_LISP) does not appear to be a valid quicklisp install)
$(error Please point QUICK_LISP to your quicklisp installation)
endif

# Lisp Libraries to compile into executables
#
#  Set the LISP_LIBS env variable to load another lisp system into the
#  compiled optimization executables.
#
LISP_LIBS+= optimize
LC_LIBS:=$(addprefix --load-system , $(LISP_LIBS))

# Flags to buildapp
QUIT=(lambda (error hook-value)
QUIT+=(declare (ignorable hook-value))
QUIT+=(format *error-output* \"ERROR: ~a~%\" error)
QUIT+=\#+sbcl (sb-ext:exit :code 2) \#+ccl (quit 2))
LCFLAGS=--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--asdf-tree $(QUICK_LISP)/dists/quicklisp/software \
	--eval "(setf *debugger-hook* $(QUIT))" \
	$(LC_LIBS) \
	--eval "(setf optimize::*git-version* \"$$(git describe --always)\")"

ifneq ($(LISP_STACK),)
LCFLAGS+= --dynamic-space-size $(LISP_STACK)
endif

# Compiled lisp executables
LISP_EXES=goa objread annotate delta neutral
LISP_BINS=$(addprefix bin/, $(LISP_EXES))

# Compiled C executables
C_EXES=limit no-limit
C_BINS=$(addprefix bin/, $(C_EXES))

all: $(C_BINS) $(LISP_BINS)
.PHONY:  clean

bin/limit: bin/limit.c
	$(CC) $< -o $@

bin/no-limit: bin/no-limit.c
	$(CC) $< -o $@

bin/%: src/package.lisp src/optimize.lisp src/%.lisp
	$(LC) $(LCFLAGS) --output $@ --entry "optimize:$*"

clean:
	rm -f $(C_BINS) $(LISP_BINS) **/*.fasl **/*.lx32fsl
