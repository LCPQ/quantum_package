.NOPARALLEL:

# Check if QPACKAGE_ROOT is defined
ifndef QPACKAGE_ROOT
$(info  -------------------- Error --------------------)
$(info  QPACKAGE_ROOT undefined. Source the quantum_package.rc script)
$(info  -----------------------------------------------)
$(error )
endif


LIBS=
PKGS=
OCAMLCFLAGS="-g -warn-error A"
OCAMLBUILD=ocamlbuild -j 0 -syntax camlp4o -cflags $(OCAMLCFLAGS) -lflags $(OCAMLCFLAGS) 
MLFILES=$(wildcard *.ml) ezfio.ml Qptypes.ml
MLIFILES=$(wildcard *.mli) 
ALL_TESTS=$(patsubst %.ml,%.byte,$(wildcard test_*.ml))
ALL_EXE=$(patsubst %.ml,%.native,$(wildcard qp_*.ml))

.PHONY: executables default


default: $(ALL_TESTS) $(ALL_EXE) .gitignore

.gitignore: $(MLFILES)
	@for i in .gitignore ezfio.ml Qptypes.ml qptypes_generator.byte _build $(ALL_EXE) $(ALL_TESTS) \
	$(patsubst %.ml,%,$(wildcard test_*.ml)) $(patsubst %.ml,%,$(wildcard qp_*.ml)) \
	$(shell grep Input Input_auto_generated.ml | awk '{print $$2 ".ml"}') \
	Input_auto_generated.ml;\
	do \
	   echo $$i ; \
	done > .gitignore

executables: $(QPACKAGE_ROOT)/data/executables

$(QPACKAGE_ROOT)/data/executables:
	$(QPACKAGE_ROOT)/scripts/module/create_executables_list.sh

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

qp_run.native: $(MLFILES) $(MLIFILES) executables

%.native: $(MLFILES) $(MLIFILES)
	rm -f -- $*
	$(OCAMLBUILD) $*.native -use-ocamlfind $(PKGS)
	ln -s $*.native $*

qptypes_generator.byte: qptypes_generator.ml
	$(OCAMLBUILD) qptypes_generator.byte  -use-ocamlfind 

Qptypes.ml: qptypes_generator.byte
	./qptypes_generator.byte > Qptypes.ml

clean: 
	rm -rf _build Qptypes.ml $(ALL_EXE) $(ALL_TESTS)
