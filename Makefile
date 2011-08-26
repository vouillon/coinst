
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep

OBJS = util.cmx common.cmx dgraph.cmx solver.cmx api.cmx deb_lib.cmx rpm_lib.cmx repository.cmx quotient.cmx conflicts.cmx graph.cmx main.cmx
COMPFLAGS=-package unix,str,cudf
OPTLINKFLAGS=$(COMPFLAGS) -linkpkg

all: check_coinstall

check_coinstall: $(OBJS)
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

check_coinstall.byte: $(OBJS:.cmx=.cmo)
	$(OCAMLC) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

clean::
	rm -f check_coinstall

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
	$(OCAMLDEP) $(DEPFLAGS) *.ml *.mli > .depend

include .depend
