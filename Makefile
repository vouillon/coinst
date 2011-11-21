VERSION=1.01
COINST=coinst
UPGRADE=upgrade
TRANS=transition

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep

TASK = bytearray_stubs.o bytearray.cmx task_stubs.o task.cmx
OBJS = util.cmx file.cmx debug.cmx common.cmx cache.cmx layout.cmx \
       solver.cmx api.cmx deb_lib.cmx rpm_lib.cmx \
       repository.cmx quotient.cmx conflicts.cmx graph.cmx coinst_common.cmx
COMPFLAGS=-package unix,str,bigarray -g
OPTLINKFLAGS=$(COMPFLAGS) -linkpkg

all: $(COINST) $(UPGRADE) $(TRANS)

$(COINST): $(OBJS) main.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

$(COINST).byte: $(OBJS:.cmx=.cmo) main.cmo
	$(OCAMLC) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

$(UPGRADE): $(OBJS) upgrade_common.cmx upgrade.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

$(TRANS): $(OBJS) $(TASK) upgrade_common.cmx horn.cmx transition.cmx
	$(OCAMLOPT) -o $@  $(OPTLINKFLAGS) $^ $(LINKFLAGS)

clean::
	rm -f $(COINST)

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
	find . -maxdepth 1 -name "*.ml" -o -name "*.mli" | \
        grep -v download.ml | xargs $(OCAMLDEP) $(DEPFLAGS) > .depend

include .depend

####

release:
	darcs dist -d coinst-$(VERSION)
