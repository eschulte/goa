GENPROG=/home/eschulte/research/epr/code

OCAML_OPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_LINUX \
  -I $(GENPROG)/src \
  -I $(GENPROG)/obj

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)

.PHONY: clean

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

optimize: optimize.cmx
	$(OCAMLOPT) -o $@ str.cmxa unix.cmxa genprog.cmxa cil.cmxa $^

clean:
	rm -f optimize *.mli *.cmo *.cmi *.d *.cmx *.dx *.o
