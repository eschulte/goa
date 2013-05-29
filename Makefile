# Path to SBCL buildapp executable, http://www.xach.com/lisp/buildapp/
BA:=buildapp

# Size of the lisp memory in Mb, defaults to 2G
LISP_STACK?=2048

# Pointer to local Quicklisp directory
QUICK_LISP?=$(HOME)/quick-lisp

# Lisp Libraries to compile into optimize
#
#  Set the LL env variable to load another lisp system for use in a
#  config file.
#
LISP_LIBRARIES+=$(LL)
LISP_LIBRARIES+=software-evolution
LISP_LIBRARIES+=cl-store
LISP_LIBRARIES+=split-sequence
LISP_LIBRARIES+=cl-ppcre

LISP_LIBRARIES:=$(addprefix --load-system , $(LISP_LIBRARIES))

ifeq ($(shell [ -f $(QUICK_LISP)/setup.lisp ] && echo exists),)
$(error The QUICK_LISP environment variable must point to your quicklisp install)
endif

BUILD_APP_FLAGS= --manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--manifest-file etc/data/ql-manifest.txt \
	$(LISP_LIBRARIES) \
	--dynamic-space-size $(LISP_STACK)

all: bin/no-limit bin/no-stack-limit bin/limit bin/optimize

etc/data/ql-manifest.txt:
	sbcl --eval '(progn (ql:write-asdf-manifest-file "$@") (sb-ext:exit))'

bin/optimize: src/run-optimize.lisp etc/data/ql-manifest.txt
	$(BA) $(BUILD_APP_FLAGS) --load $< --output $@ --entry "optimize:main"

bin/calc-energy: src/calc-energy.lisp etc/data/ql-manifest.txt
	$(BA) $(BUILD_APP_FLAGS) --load $< --output $@ --entry "optimize:main"

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit bin/optimize
