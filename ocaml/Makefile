#TODO : Opam auto-installer in makefile

# Check if QPACKAGE_ROOT is defined

ifndef QPACKAGE_ROOT
$(info  -------------------- Error --------------------)
$(info  QPACKAGE_ROOT undefined. Source the quantum_package.rc script)
$(info  -----------------------------------------------)
$(error )
endif


LIBS=
PKGS=
OCAMLCFLAGS=-g 
OCAMLBUILD=ocamlbuild -j 0 -cflags $(OCAMLCFLAGS) -lflags -g 
MLFILES=$(wildcard *.ml) ezfio.ml Qptypes.ml
MLIFILES=$(wildcard *.mli) 
ALL_TESTS=$(patsubst %.ml,%.byte,$(wildcard test_*.ml))
ALL_EXE=$(patsubst %.ml,%.native,$(wildcard qp_*.ml))

.PHONY: executables default

default: $(ALL_TESTS) $(ALL_EXE)

executables: $(ALL_EXE)

%.inferred.mli: $(MLFILES)
	$(OCAMLBUILD) $*.inferred.mli -use-ocamlfind  $(PKGS) 
	mv _build/$*.inferred.mli .

%.byte: $(MLFILES) $(MLIFILES)
	rm -f -- $*
	$(OCAMLBUILD) $*.byte  -use-ocamlfind  $(PKGS)
	ln -s $*.byte $*

%.native: $(MLFILES) $(MLIFILES)
	rm -f -- $*
	$(OCAMLBUILD) $*.native -use-ocamlfind $(PKGS)
	ln -s $*.native $*

ezfio.ml: ${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml
	cp ${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml .

qptypes_generator.byte: qptypes_generator.ml
	$(OCAMLBUILD) qptypes_generator.byte  -use-ocamlfind 

Qptypes.ml: qptypes_generator.byte
	./qptypes_generator.byte > Qptypes.ml
	rm qptypes_generator.byte

${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml:
	$(MAKE) -C ${QPACKAGE_ROOT}/src ezfio

clean: 
	rm -rf _build *.native *.byte
