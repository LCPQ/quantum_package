all_ezfio: $(shell for i in *.md5 ; do echo $${i/md5/ezfio} ; done)
	@printf '\nCreated EZFIO directories\n'

all_md5: $(shell for i in *.ezfio ; do echo $${i/ezfio/md5} ; done)
	@printf '\nCreated md5 files\n'

FORCE:

%.md5: FORCE
	@archive_ezfio.sh $*.ezfio
	@printf '.'

%.ezfio: FORCE
	@unarchive_ezfio.sh $*.md5
	@printf '.'
