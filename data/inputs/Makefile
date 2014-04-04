all_ezfio: $(shell for i in *.out.ezfio ; do echo $$i ; done)
all_md5: $(shell for i in *.out.ezfio.md5 ; do echo $$i ; done)

FORCE:

%.ezfio: FORCE
	archive_ezfio.sh $@

%.ezfio.md5: FORCE
	unarchive_ezfio.sh $@
