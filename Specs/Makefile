
OBJS=Po.vo Lattice.vo Relation.vo

all: $(OBJS)

########################################################################
# Coq proofs

COQC=coqc

%.vo : %.v
	$(COQC) $<

######################################################################
# Dependencies

COQDEP=coqdep

depend:
	$(COQDEP) *.v > .depend

include .depend
