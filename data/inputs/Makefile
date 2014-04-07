all_ezfio: $(shell for i in *.out.ezfio.md5 ; do echo $$(basename $$i .md5) ; done)
all_md5: $(shell for i in *.out.ezfio ; do echo $$i.md5 ; done)

FORCE:

%.ezfio.md5: FORCE
	archive_ezfio.sh $(basename $@ .md5)

%.ezfio: FORCE
	unarchive_ezfio.sh $@.md5
