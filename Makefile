VERSION=1.01
COINST=coinst
UPGRADE=coinst-upgrades
TRANS=comigrate

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

TASK = bytearray_stubs.o bytearray.cmx task_stubs.o task.cmx
SVG=viewer/scene.cmx viewer/dot_parser.cmx viewer/dot_lexer.cmx \
    viewer/dot_graph.cmx viewer/dot_render.cmx viewer/scene_svg.cmx \
    viewer/dot_file.cmx
OBJS = ptset.cmx util.cmx file.cmx debug.cmx common.cmx cache.cmx layout.cmx \
       solver.cmx api.cmx deb_lib.cmx rpm_lib.cmx \
       repository.cmx quotient.cmx conflicts.cmx graph.cmx json.cmx coinst_common.cmx
COMPFLAGS=-package unix,str,bigarray,cudf -g -I viewer
OPTLINKFLAGS=$(COMPFLAGS) -linkpkg

OCAMLDEP=ocamlfind ocamldep
DEPFLAGS = -package js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o -I viewer

all: $(COINST) $(UPGRADE) $(TRANS)

$(COINST): $(OBJS) cudf_lib.cmx coinst.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

$(COINST).byte: $(OBJS:.cmx=.cmo) cudf_lib.cmo coinst.cmo
	$(OCAMLC) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)


$(UPGRADE): $(OBJS) $(TASK) $(SVG) upgrade_common.cmx upgrade.cmx upgrade_main.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

$(TRANS): $(OBJS) $(TASK) $(SVG) update_data.cmx upgrade_common.cmx upgrade.cmx horn.cmx transition.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

clean::
	rm -f $(COINST) $(UPGRADE) $(TRANS)

#####

clean::
	find . -regex ".*\\.\(cm[oix]\|o\)" | xargs rm -f

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mly .mll .idl .o .c

.ml.cmx:
	$(OCAMLOPT) $(OPTCOMPFLAGS) $(COMPFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(BYTECOMPFLAGS) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.idl.ml:
	$(OCAMLIDL) $<

.mly.ml:
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<

.mll.ml:
	$(OCAMLLEX) $<

.c.o:
	$(OCAMLC) -ccopt "-o $@" $(COMPFLAGS) -ccopt "$(CFLAGS)" -c $<

depend:
	ls *.ml *.mli viewer/*.ml viewer/*.mli | \
        xargs $(OCAMLDEP) $(DEPFLAGS) > .depend

include .depend

####

release:
	darcs dist -d coinst-$(VERSION)
