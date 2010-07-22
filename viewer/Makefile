
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

COMPFLAGS=-package str,cairo.lablgtk2,js_of_ocaml -syntax camlp4o

GENERATED=dot_parser.ml dot_lexer.ml
OBJS=scene.cmx viewer_common.cmx viewer.cmx \
     dot_file.cmx dot_parser.cmx dot_lexer.cmx dot_graph.cmx dot_render.cmx \
     main.cmx
OPTLINKFLAGS=-package str,cairo.lablgtk2 -linkpkg

JSOBJS=scene.cmo viewer_common.cmo viewer_js.cmo
LINKFLAGS=-package js_of_ocaml -linkpkg

all: viewer jsviewer.js

viewer: $(OBJS)
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^

jsviewer.js: jsviewer.byte
	js_of_ocaml $^

jsviewer.byte: $(JSOBJS)
	$(OCAMLC) -o $@ $(LINKFLAGS) $^

clean::
	rm -f check_coinstall

#####

clean::
	find . -regex ".*\\.\(cm[oix]\|o\)" | xargs rm -f

%.cmx: %.ml
	$(OCAMLOPT) $(OPTCOMPFLAGS) $(COMPFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmo: %.ml
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
