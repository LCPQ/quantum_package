WWW_SERVER = http://qmcchem.ups-tlse.fr/files/scemama
IRPF90_TGZ = irpf90-latest-noarch-src.tar.gz
EZFIO_TGZ  = EZFIO.latest.tar.gz

.PHONY: doc src

default: src

EZFIO: 
	$(info ===== Fetching EZFIO from the web =====)
	@wget "$(WWW_SERVER)/$(EZFIO_TGZ)" || \
	  (echo Unable to download EZFIO : $(WWW_SERVER)/$(EZFIO_TGZ) ; exit 1)
	@tar -zxf $(EZFIO_TGZ) && rm $(EZFIO_TGZ)

irpf90:
	$(info ===== Fetching IRPF90 from the web =====)
	@wget "$(WWW_SERVER)/$(IRPF90_TGZ)" || \
	  (echo Unable to download IRPF90 : $(WWW_SERVER)/$(IRPF90_TGZ) ; exit 1)
	@tar -zxf $(IRPF90_TGZ) && rm $(IRPF90_TGZ)
	$(MAKE) -C irpf90

doc:
	$(MAKE) -C doc

src: irpf90 EZFIO
	export QPACKAGE_ROOT=$$PWD ; \
	$(MAKE) -C src
