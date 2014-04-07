all_ezfio: $(shell ls *.md5 | sed 's/\.md5/\.ezfio/')
	@printf '\nCreated EZFIO directories\n'

all_md5: $(shell ls -d *.ezfio | sed 's/\.ezfio/\.md5/')
	@printf '\nCreated md5 files\n'

FORCE:

%.md5: FORCE
	@archive_ezfio.sh $*.ezfio
	@printf '.'

%.ezfio: FORCE
	@unarchive_ezfio.sh $*.md5
	@printf '.'
