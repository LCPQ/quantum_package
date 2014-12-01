FC=ifort -O2 -xHost -openmp
#FC=gfortran -g -O2 -Wall -Wno-unused -fopenmp
LIBS=-lzmq libf77zmq.a

EXE=hwserver hwclient wuserver wuclient ventserver ventclient\
    hwserver_msg hwclient_msg socket_monitor dummy_cipsi

.PHONY: default

default: $(EXE)

%: %.f f77_zmq.h libf77zmq.a
	$(FC) $*.f -o $@ $(LIBS)

libf77zmq.a: ../libf77zmq.a
	$(MAKE) -C ..
	cp ../libf77zmq.a .

f77_zmq.h: ../f77_zmq.h
	cp ../f77_zmq.h .

clean:
	$(RM) -f -- $(EXE) f77_zmq.o f77_zmq.h *.ipc
