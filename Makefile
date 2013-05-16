# Path to SBCL buildapp executable, http://www.xach.com/lisp/buildapp/
BA:=buildapp

# Size of the lisp memory in Mb, defaults to 2G
LISP_STACK:=2048

all: bin/no-limit bin/no-stack-limit bin/limit bin/optimize

bin/optimize: src/run-optimize.lisp
	$(BA) --output $@ --entry optimize:main --load $< --dynamic-space-size $(LISP_STACK)

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit bin/optimize
