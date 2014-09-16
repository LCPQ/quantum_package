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
OCAMLBUILD=ocamlbuild -cflags $(OCAMLCFLAGS) -lflags -g
MLFILES=$(wildcard *.ml) ezfio.ml Qptypes.ml
MLIFILES=$(wildcard *.mli) 
ALL_TESTS=$(patsubst %.ml,%.byte,$(wildcard test_*.ml))
ALL_EXE=$(patsubst %.ml,%.native,$(wildcard qp_*.ml))


default: $(ALL_TESTS) executables

executables: $(ALL_EXE)

%.inferred.mli: $(MLFILES)
	$(OCAMLBUILD) $*.inferred.mli -cflags -i -use-ocamlfind  $(PKGS)

%.byte: $(MLFILES) $(MLIFILES)
	$(OCAMLBUILD) $*.byte  -use-ocamlfind  $(PKGS)

%.native: $(MLFILES) $(MLIFILES)
	$(OCAMLBUILD) $*.native -use-ocamlfind $(PKGS)

ezfio.ml: ${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml
	cp ${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml .

qptypes_generator.byte: qptypes_generator.ml
	$(OCAMLBUILD) qptypes_generator.byte  -use-ocamlfind 

qptypes.ml: qptypes_generator.byte
	./qptypes_generator.byte > Qptypes.ml
	rm qptypes_generator.byte

${QPACKAGE_ROOT}/EZFIO/Ocaml/ezfio.ml:
	$(MAKE) -C ${QPACKAGE_ROOT}/src ezfio

clean: 
	rm -rf _build *.native *.byte
