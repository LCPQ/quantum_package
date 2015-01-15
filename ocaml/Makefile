# Check if QPACKAGE_ROOT is defined

.NOPARALLEL:

ifndef QPACKAGE_ROOT
$(info  -------------------- Error --------------------)
$(info  QPACKAGE_ROOT undefined. Source the quantum_package.rc script)
$(info  -----------------------------------------------)
$(error )
endif


LIBS=
PKGS=
OCAMLCFLAGS="-g -warn-error A"
OCAMLBUILD=ocamlbuild -j 0 -syntax camlp4o -cflags $(OCAMLCFLAGS) -lflags $(OCAMLCFLAGS) -ocamlopt ocamlc.opt
MLFILES=$(wildcard *.ml) ezfio.ml Qptypes.ml
MLIFILES=$(wildcard *.mli) 
ALL_TESTS=$(patsubst %.ml,%.byte,$(wildcard test_*.ml))
ALL_EXE=$(patsubst %.ml,%.native,$(wildcard qp_*.ml))

.PHONY: executables default


default: $(ALL_TESTS) $(ALL_EXE) .gitignore

.gitignore: $(MLFILES)
	@for i in .gitignore ezfio.ml Qptypes.ml qptypes_generator.byte _build $(ALL_EXE) $(ALL_TESTS) \
	$(patsubst %.ml,%,$(wildcard test_*.ml)) $(patsubst %.ml,%,$(wildcard qp_*.ml)) ; do \
	   echo $$i ; \
	done >> .gitignore

executables: 
	$(MAKE) -C $(QPACKAGE_ROOT)/data executables

external_libs:
	opam install cryptokit core

qpackage.odocl: $(MLIFILES)
	ls $(MLIFILES) | sed "s/\.mli//" > qpackage.odocl

doc: qpackage.odocl
	$(OCAMLBUILD) qpackage.docdir/index.html -use-ocamlfind  $(PKGS) 

%.inferred.mli: $(MLFILES)
	$(OCAMLBUILD) $*.inferred.mli -use-ocamlfind  $(PKGS) 
	mv _build/$*.inferred.mli .

%.byte: $(MLFILES) $(MLIFILES)
	rm -f -- $*
	$(OCAMLBUILD) $*.byte  -use-ocamlfind  $(PKGS)
	ln -s $*.byte $*

%.native: $(MLFILES) $(MLIFILES) executables
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
	rm -rf _build Qptypes.ml $(ALL_EXE) $(ALL_TESTS)
