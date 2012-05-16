OCAML_OPTIONS = \
  -I $(CIL)/ \
  -I $(CIL)/src \
  -I $(CIL)/src/ext \
  -I $(CIL)/src/frontc \
  -I $(CIL)/obj/x86_LINUX \
  -I $(GENPROG)/src

OCAMLC   = ocamlc -g $(OCAML_OPTIONS)
OCAMLOPT = ocamlopt -w Aelzv $(OCAML_OPTIONS)
OCAMLDEP = ocamldep $(OCAML_OPTIONS)

%.cmx: %.ml 
	@if [ -f $*.mli -a ! -f $*.cmi ] ; then $(OCAMLC) -c -g $*.mli ; fi 
	$(OCAMLOPT) -c $*.ml
	@$(OCAMLDEP) $*.ml > $*.d 

optimize: optimize.cmx
	$(OCAMLOPT) -o $@ bigarray.cmxa unix.cmxa str.cmxa nums.cmxa cil.cmxa \
	global.cmx asmrep.cmx gaussian.cmx rep.cmx stringrep.cmx stats2.cmx $^
