
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

GENERATED=dot_parser.ml dot_lexer.ml
OBJS=scene.cmx viewer.cmx dot_file.cmx dot_parser.cmx dot_lexer.cmx dot_graph.cmx dot_render.cmx main.cmx

COMPFLAGS=-package str,cairo.lablgtk2
OPTLINKFLAGS=$(COMPFLAGS) -linkpkg


all: viewer

viewer: $(OBJS)
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

clean::
	rm -f check_coinstall

#####

clean::
	find . -regex ".*\\.\(cm[oix]\|o\)" | xargs rm -f

%.cmx: %.ml
	$(OCAMLOPT) $(OPTCOMPFLAGS) $(COMPFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<

%.ml: %.mly
	$(OCAMLYACC) $<

%.mli: %.mly
	$(OCAMLYACC) $<

%.ml: %.mll
	$(OCAMLLEX) $<

depend: $(GENERATED)
	find . -regex ".*\\.mli?" | xargs \
	$(OCAMLDEP) $(DEPFLAGS) $$i \
	> .depend

include .depend
