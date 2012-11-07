CC := guild compile

all: run
.PHONY: run

%.go: %.scm
	$(CC) -o $@ $<

run: opt.scm
	guild opt.scm; \
	guile -s opt.scm |tee output
