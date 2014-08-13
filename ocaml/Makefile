LIBS=
PKGS=
OCAMLCFLAGS=-g
OCAMLBUILD=ocamlbuild -cflags $(OCAMLCFLAGS) -lflags -g


test_elements.byte: 

%.inferred.mli: $(wildcard *.ml) 
	$(OCAMLBUILD) $*.inferred.mli -cflags -i -use-ocamlfind  $(PKGS)

%.byte: $(wildcard *.ml) $(wildcard *.mli) 
	$(OCAMLBUILD) $*.byte  -use-ocamlfind  $(PKGS)

%.native: $(wildcard *.ml) $(wildcard *.mli)  
	$(OCAMLBUILD) $*.native -use-ocamlfind $(PKGS)

clean: 
	rm -rf _build
