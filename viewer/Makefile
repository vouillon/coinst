
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

COMPFLAGS=-package str,cairo.lablgtk2,js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o
DEPFLAGS=$(COMPFLAGS)

GENERATED=dot_parser.ml dot_lexer.ml
OBJS=scene.cmx scene_extents.cmx viewer_common.cmx viewer.cmx \
     dot_file.cmx dot_parser.cmx dot_lexer.cmx dot_graph.cmx dot_render.cmx \
     main.cmx
CONVERTER=scene.cmx scene_extents.cmx scene_json.cmx \
     dot_file.cmx dot_parser.cmx dot_lexer.cmx dot_graph.cmx dot_render.cmx \
     converter.cmx
OPTLINKFLAGS=-package str,cairo.lablgtk2 -linkpkg

JSOBJS=scene.cmo viewer_common.cmo viewer_js.cmo
LINKFLAGS=-package js_of_ocaml -linkpkg

all: coinst_viewer jsviewer.js coinst_converter
opt: all
byte: coinst_viewer.byte jsviewer.js coinst_converter.byte

coinst_viewer: $(OBJS)
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^

coinst_viewer.byte: $(OBJS:.cmx=.cmo)
	$(OCAMLC) -o $@  $(OPTLINKFLAGS) $^

coinst_converter: $(CONVERTER)
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^

coinst_converter.byte: $(CONVERTER:.cmx=.cmo)
	$(OCAMLC) -o $@  $(OPTLINKFLAGS) $^

jsviewer.js: jsviewer.byte
	js_of_ocaml $^ -pretty

jsviewer.byte: $(JSOBJS)
	$(OCAMLC) -o $@ $(LINKFLAGS) $^

realclean:: clean
	rm -f dot_parser.ml dot_parser.mli dot_lexer.ml

clean::
	rm -f coinst_converter coinst_viewer coinst_viewer.byte coinst_converter.byte
	rm -f jsviewer.js jsviewer.byte
	rm -f dot_lexer.ml dot_parser.ml

#####

clean::
	find . -regex ".*\\.\(cm[toix].?\|o\|annot\)" | xargs rm -f

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
