.NOPARALLEL:

# Check if QP_ROOT is defined
ifndef QP_ROOT
$(info  -------------------- Error --------------------)
$(info  QP_ROOT undefined. Source the quantum_package.rc script)
$(info  -----------------------------------------------)
$(error )
endif


LIBS=
PKGS=
OCAMLCFLAGS="-g"
OCAMLBUILD=ocamlbuild -j 0 -cflags $(OCAMLCFLAGS) -lflags $(OCAMLCFLAGS) 
MLLFILES=$(wildcard *.mll) 
MLFILES=$(wildcard *.ml) ezfio.ml Qptypes.ml Input_auto_generated.ml qp_edit.ml 
MLIFILES=$(wildcard *.mli) git
ALL_TESTS=$(patsubst %.ml,%.byte,$(wildcard test_*.ml))
ALL_EXE=$(patsubst %.ml,%.native,$(wildcard qp_*.ml)) qp_edit.native

.PHONY: executables default remake_executables


default: $(ALL_EXE) .gitignore
tests: $(ALL_TESTS) 

.gitignore: $(MLFILES) $(MLIFILES)
	@for i in .gitignore ezfio.ml Qptypes.ml Git.ml qptypes_generator.byte _build $(ALL_EXE) $(ALL_TESTS) \
	$(patsubst %.ml,%,$(wildcard test_*.ml)) $(patsubst %.ml,%,$(wildcard qp_*.ml)) \
	$(shell grep Input Input_auto_generated.ml | awk '{print $$2 ".ml"}') \
	qp_edit.ml qp_edit qp_edit.native Input_auto_generated.ml;\
	do \
	   echo $$i ; \
	done |sort | uniq > .gitignore

executables: $(QP_ROOT)/data/executables


$(QP_ROOT)/data/executables: remake_executables
	$(QP_ROOT)/scripts/module/create_executables_list.sh

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

ezfio.ml: ${QP_ROOT}/install/EZFIO/Ocaml/ezfio.ml
	cp ${QP_ROOT}/install/EZFIO/Ocaml/ezfio.ml .

qptypes_generator.byte: qptypes_generator.ml
	$(OCAMLBUILD) qptypes_generator.byte  -use-ocamlfind 

Qptypes.ml: qptypes_generator.byte
	./qptypes_generator.byte > Qptypes.ml

git: 
	./create_git_sha1.sh

${QP_ROOT}/install/EZFIO/Ocaml/ezfio.ml: 
	$(NINJA) -C ${QP_ROOT}/install/EZFIO

Input_auto_generated.ml qp_edit.ml: $(filter-out Input_auto_generated.ml, $(wildcard Input_*.ml))
	ei_handler.py ocaml_global

clean: 
	rm -rf _build Qptypes.ml Git.ml Input_auto_generated.ml $(ALL_EXE) $(ALL_TESTS)

