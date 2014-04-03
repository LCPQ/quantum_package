all: $(wildcard *.ezfio)

FORCE:

%.ezfio.md5: FORCE
	archive_ezfio.sh $*.ezfio

%.ezfio: FORCE
	unarchive_ezfio.sh $@.md5
