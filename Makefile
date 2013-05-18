# Path to SBCL buildapp executable, http://www.xach.com/lisp/buildapp/
BA:=buildapp

# Size of the lisp memory in Mb, defaults to 2G
LISP_STACK?=2048

# Pointer to local Quicklisp directory
QUICK_LISP?=$(HOME)/quick-lisp

ifeq ($(shell [ -d $(QUICK_LISP) ] && echo exists),)
$(error The QUICK_LISP environment variable must point to your quicklisp install)
endif

all: bin/no-limit bin/no-stack-limit bin/limit bin/optimize

etc/data/ql-manifest.txt:
	sbcl --eval '(progn (ql:write-asdf-manifest-file "$@") (sb-ext:exit))'

bin/optimize: src/run-optimize.lisp etc/data/ql-manifest.txt
	$(BA) \
	--manifest-file $(QUICK_LISP)/local-projects/system-index.txt \
	--manifest-file etc/data/ql-manifest.txt \
	--load-system software-evolution \
	--load-system cl-store \
	--load-system split-sequence \
	--load-system cl-ppcre \
	--dynamic-space-size $(LISP_STACK) \
	--load $< --entry optimize:main --output $@

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit bin/optimize
