# Path to SBCL buildapp executable, http://www.xach.com/lisp/buildapp/
CLC:=cl-launch
LISP:="sbcl ccl"

# Compiled lisp executables
LISP_EXES=optimize objread calc-energy
LISP_BINS=$(addprefix bin/, $(LISP_EXES))

# Flags to build standalone executables
CLFLAGS=--no-include --system optimize --lisp $(LISP) --dump '!' -f etc/cl-launch.lisp

all: bin/no-limit bin/no-stack-limit bin/limit $(LISP_BINS)

bin/optimize: src/run-optimize.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:main

bin/objread: src/objread.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:objread

bin/calc-energy: src/calc-energy.lisp
	$(CLC) $(CLFLAGS) --output $@ -r optimize:calc-energy

clean:
	rm -f bin/no-limit bin/no-stack-limit bin/limit $(LISP_BINS)

real-clean: clean
	rm -f **/*.fasl **/*.lx32fsl
