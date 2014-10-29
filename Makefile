WWW_SERVER = http://qmcchem.ups-tlse.fr/files/scemama
IRPF90_TGZ = irpf90-latest-noarch-src.tar.gz
EZFIO_TGZ  = EZFIO.latest.tar.gz
FETCH_FROM_WEB=./scripts/fetch_from_web.py

BLUE=[34m
BLACK=(B[m


.PHONY: doc src curl m4 ocaml irpf90 

default: 
	exec ./setup_environment.sh

curl: bin/curl
m4: bin/m4
irpf90: bin/irpf90

EZFIO: bin/irpf90
	$(info $(BLUE)===== Fetching EZFIO from the web ===== $(BLACK))
	@sleep 1
	@$(FETCH_FROM_WEB) "$(WWW_SERVER)/$(EZFIO_TGZ)" $(EZFIO_TGZ) || \
	  (echo Unable to download EZFIO : $(WWW_SERVER)/$(EZFIO_TGZ) ; exit 1)
	tar -zxf $(EZFIO_TGZ) && rm $(EZFIO_TGZ)
	$(MAKE) -C src $$PWD/EZFIO
	touch EZFIO

bin/irpf90:
	$(info $(BLUE)===== Fetching IRPF90 from the web ===== $(BLACK))
	@sleep 1
	@$(FETCH_FROM_WEB) "$(WWW_SERVER)/$(IRPF90_TGZ)" $(IRPF90_TGZ) || \
	  (echo Unable to download IRPF90 : $(WWW_SERVER)/$(IRPF90_TGZ) ; exit 1)
	tar -zxf $(IRPF90_TGZ) && rm $(IRPF90_TGZ)
	$(MAKE) -C irpf90 | tee install_irpf90.log
	ln -s $$PWD/irpf90/bin/irpf90 $$PWD/bin/irpf90
	ln -s $$PWD/irpf90/bin/irpman $$PWD/bin/irpman

doc:
	$(MAKE) -C doc

src: irpf90 EZFIO ocaml
	@export QPACKAGE_ROOT=$$PWD ; \
	$(MAKE) -C src

bin/curl:
	$(info $(BLUE)===== Installing curl =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_curl.sh | tee install_curl.log

bin/m4:
	$(info $(BLUE)===== Installing m4 =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_m4.sh | tee install_m4.log


ocaml: curl m4
	- rm -f -- ocaml/Qptypes.ml
	$(MAKE) ocaml/Qptypes.ml

ocaml/Qptypes.ml: 
	$(info $(BLUE)===== Installing ocaml =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_ocaml.sh | tee install_ocaml.log

