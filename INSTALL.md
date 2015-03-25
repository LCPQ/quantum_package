# Installation

## Requirements

* curl
* m4
* GNU make
* Fortran compiler (ifort or gfortran)
* Python 2.7 or new
* Bash


## Installing <<Normaly>>

1) Run `./setup_environment.sh`
    It will doawnload and install all the requirement
    (Installing OCaml will take somme time 20min)

2) `source /home/razoa/quantum_package/quantum_package.rc`
    It containt all the environement variable neeeded by the quantum package

3) Create the Makefile.config who containt all the flag needed by the compilator.
    (`cp ./src/Makefile.config.gfortran  ./src/Makefile.config`)

4) make build
    It will compile all the fortran

## Installing behind a firewall

1) Download `tsocks`:

    wget http://sourceforge.net/projects/tsocks/files/latest/download
    mv download tsocks.tar.gz

2) Tranfer `tsocks.tar.gz` on the remote host

3) Configure `tsocks` with the proper directory for the `tsocks.conf` file:

    tar -zxvf tsocks.tar.gz
    cd tsocks-*
    ./configure --with-conf=${PWD}/tsocks.conf

4) Create the `tsocks.conf` file with the following content:

    server = 127.0.0.1
    server_port = 10000

5) Create the tsocks library:

    make

6) Add the `libtsocks.so` to the `LD_PRELOAD` environment variable:

    export LD_PRELOAD="${PWD}/libtsocks.so.1.8"

7) Create a custom curl command to set the tsocks option: open a file named
   `curl`, which is accessible from your `PATH` environment variable before the
   real `curl` command, and fill this file with:

    #!/bin/bash
    /usr/bin/curl --socks5 127.0.0.1:10000 $@

8) Start a tsocks ssh tunnel:

    ssh -fN -D 10000 user@external-server.com


